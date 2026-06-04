(function () {
  const defaultDebounceMs = 1000;
  const saveTimers = new Map();
  const subscriptions = new Set();
  let app = null;
  let socket = null;
  let reconnectTimer = null;

  function config() {
    return window.TodoGraphSyncConfig || {};
  }

  function apiUrl(path) {
    const base = config().apiBase || "/api";
    return base.replace(/\/$/, "") + path;
  }

  function websocketUrl() {
    if (config().wsUrl) {
      return config().wsUrl;
    }

    const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
    return protocol + "//" + window.location.host + "/ws";
  }

  function sendPort(portName, payload) {
    if (app && app.ports[portName]) {
      app.ports[portName].send(payload);
    }
  }

  function projectPath(projectId) {
    return "/projects/" + encodeURIComponent(projectId);
  }

  function reportSyncFailure(operation, projectId, error, status) {
    const message = error && error.message ? error.message : String(error);

    sendPort("syncFailed", {
      operation,
      projectId: projectId || null,
      message,
      status: status || null,
    });
  }

  async function responseJson(response) {
    try {
      return await response.json();
    } catch (_error) {
      return null;
    }
  }

  function validSaveEnvelope(envelope) {
    return (
      envelope &&
      typeof envelope.projectId === "string" &&
      Number.isInteger(envelope.updatedAt) &&
      Object.prototype.hasOwnProperty.call(envelope, "payload")
    );
  }

  async function saveProject(envelope) {
    if (!validSaveEnvelope(envelope)) {
      reportSyncFailure("saveProject", null, new Error("Invalid save envelope"));
      return;
    }

    const projectId = envelope.projectId;

    try {
      const response = await fetch(apiUrl(projectPath(projectId)), {
        method: "PUT",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          updatedAt: envelope.updatedAt,
          payload: envelope.payload,
        }),
      });
      const body = await responseJson(response);

      if (response.ok) {
        sendPort("serverProjectSaveAccepted", body);
        return;
      }

      if (response.status === 400 || response.status === 409) {
        sendPort(
          "serverProjectSaveRejected",
          Object.assign(
            {
              projectId,
            },
            body || {
              status: "rejected",
              reason: response.status === 409 ? "stale_update" : "invalid_updated_at",
            }
          )
        );
        return;
      }

      reportSyncFailure(
        "saveProject",
        projectId,
        new Error("Server returned " + response.status),
        response.status
      );
    } catch (error) {
      reportSyncFailure("saveProject", projectId, error);
    }
  }

  function debounceSaveProject(envelope) {
    if (!validSaveEnvelope(envelope)) {
      reportSyncFailure("debounceSaveProject", null, new Error("Invalid save envelope"));
      return;
    }

    const existingTimer = saveTimers.get(envelope.projectId);

    if (existingTimer) {
      window.clearTimeout(existingTimer);
    }

    const delay = Number(config().debounceMs || defaultDebounceMs);
    const timer = window.setTimeout(() => {
      saveTimers.delete(envelope.projectId);
      saveProject(envelope);
    }, delay);

    saveTimers.set(envelope.projectId, timer);
  }

  async function fetchProject(projectId) {
    try {
      const response = await fetch(apiUrl(projectPath(projectId)));
      const body = await responseJson(response);

      if (response.ok) {
        sendPort("serverProjectLoaded", body);
        return;
      }

      reportSyncFailure(
        "fetchProject",
        projectId,
        new Error("Server returned " + response.status),
        response.status
      );
    } catch (error) {
      reportSyncFailure("fetchProject", projectId, error);
    }
  }

  async function fetchProjectVersion(projectId) {
    try {
      const response = await fetch(apiUrl(projectPath(projectId) + "/version"));
      const body = await responseJson(response);

      if (response.ok && body) {
        sendPort("serverProjectVersionLoaded", {
          projectId: body.projectId || projectId,
          exists: true,
          updatedAt: body.updatedAt,
        });
        return;
      }

      if (response.status === 404) {
        sendPort("serverProjectVersionLoaded", {
          projectId,
          exists: false,
        });
        return;
      }

      reportSyncFailure(
        "fetchProjectVersion",
        projectId,
        new Error("Server returned " + response.status),
        response.status
      );
    } catch (error) {
      reportSyncFailure("fetchProjectVersion", projectId, error);
    }
  }

  async function checkProjects(projects) {
    if (!Array.isArray(projects) || projects.length === 0) {
      sendPort("serverProjectsChecked", []);
      return;
    }

    try {
      const response = await fetch(apiUrl("/projects/check-updates"), {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(projects),
      });
      const body = await responseJson(response);

      if (response.ok && Array.isArray(body)) {
        sendPort("serverProjectsChecked", body);
        return;
      }

      reportSyncFailure(
        "checkProjects",
        null,
        new Error("Server returned " + response.status),
        response.status
      );
    } catch (error) {
      reportSyncFailure("checkProjects", null, error);
    }
  }

  function ensureSocket() {
    if (
      socket &&
      (socket.readyState === WebSocket.OPEN ||
        socket.readyState === WebSocket.CONNECTING)
    ) {
      return;
    }

    socket = new WebSocket(websocketUrl());

    socket.addEventListener("open", () => {
      for (const projectId of subscriptions) {
        sendSocketMessage({
          type: "subscribeProject",
          projectId,
        });
      }
    });

    socket.addEventListener("message", (event) => {
      let message = null;

      try {
        message = JSON.parse(event.data);
      } catch (_error) {
        return;
      }

      if (
        message &&
        message.type === "projectUpdated" &&
        typeof message.projectId === "string" &&
        Number.isInteger(message.updatedAt)
      ) {
        sendPort("serverProjectUpdated", message);
      }
    });

    socket.addEventListener("close", () => {
      socket = null;
      scheduleReconnect();
    });

    socket.addEventListener("error", () => {
      if (socket) {
        socket.close();
      }
    });
  }

  function scheduleReconnect() {
    if (subscriptions.size === 0 || reconnectTimer) {
      return;
    }

    reconnectTimer = window.setTimeout(() => {
      reconnectTimer = null;
      if (subscriptions.size > 0) {
        ensureSocket();
      }
    }, 2000);
  }

  function sendSocketMessage(message) {
    if (!socket || socket.readyState !== WebSocket.OPEN) {
      ensureSocket();
      return;
    }

    socket.send(JSON.stringify(message));
  }

  function subscribeProject(projectId) {
    if (typeof projectId !== "string" || projectId.length === 0) {
      return;
    }

    subscriptions.add(projectId);
    ensureSocket();
    sendSocketMessage({
      type: "subscribeProject",
      projectId,
    });
  }

  function unsubscribeProject(projectId) {
    if (typeof projectId !== "string" || projectId.length === 0) {
      return;
    }

    subscriptions.delete(projectId);
    sendSocketMessage({
      type: "unsubscribeProject",
      projectId,
    });

    if (subscriptions.size === 0 && socket) {
      socket.close();
    }
  }

  function init(elmApp) {
    app = elmApp;

    app.ports.checkServerProjects.subscribe(checkProjects);
    app.ports.fetchServerProject.subscribe(fetchProject);
    app.ports.fetchServerProjectVersion.subscribe(fetchProjectVersion);
    app.ports.saveServerProject.subscribe((envelope) => {
      if (envelope && envelope.projectId) {
        const timer = saveTimers.get(envelope.projectId);

        if (timer) {
          window.clearTimeout(timer);
          saveTimers.delete(envelope.projectId);
        }
      }

      saveProject(envelope);
    });
    app.ports.debounceSaveServerProject.subscribe(debounceSaveProject);
    app.ports.subscribeProject.subscribe(subscribeProject);
    app.ports.unsubscribeProject.subscribe(unsubscribeProject);
  }

  window.TodoGraphProjectSync = {
    init,
  };
})();
