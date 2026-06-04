(function () {
  const dbName = "todo-graph";
  const dbVersion = 1;
  const projectStoreName = "projects";
  let dbPromise = null;

  function reportStorageError(app, error) {
    const message = error && error.message ? error.message : String(error);

    if (app.ports.storageFailed) {
      app.ports.storageFailed.send(message);
    }
  }

  function openDb() {
    if (dbPromise) {
      return dbPromise;
    }

    if (!window.indexedDB) {
      dbPromise = Promise.reject(new Error("IndexedDB is not available"));
      return dbPromise;
    }

    dbPromise = new Promise((resolve, reject) => {
      const request = window.indexedDB.open(dbName, dbVersion);

      request.onupgradeneeded = () => {
        const db = request.result;

        if (!db.objectStoreNames.contains(projectStoreName)) {
          db.createObjectStore(projectStoreName, { keyPath: "id" });
        }
      };

      request.onsuccess = () => {
        resolve(request.result);
      };

      request.onerror = () => {
        reject(request.error || new Error("Failed to open IndexedDB"));
      };
    });

    return dbPromise;
  }

  function requestToPromise(request) {
    return new Promise((resolve, reject) => {
      request.onsuccess = () => {
        resolve(request.result);
      };

      request.onerror = () => {
        reject(request.error || new Error("IndexedDB request failed"));
      };
    });
  }

  function getAllProjects() {
    return openDb().then((db) => {
      const transaction = db.transaction(projectStoreName, "readonly");
      const store = transaction.objectStore(projectStoreName);

      return requestToPromise(store.getAll());
    });
  }

  function putProject(project) {
    return openDb().then((db) => {
      return new Promise((resolve, reject) => {
        if (!project || typeof project.id !== "string") {
          reject(new Error("Project must have a string id"));
          return;
        }

        const transaction = db.transaction(projectStoreName, "readwrite");
        const store = transaction.objectStore(projectStoreName);

        transaction.oncomplete = () => {
          resolve();
        };

        transaction.onerror = () => {
          reject(transaction.error || new Error("Failed to save project"));
        };

        store.put(project);
      });
    });
  }

  function deleteProject(projectId) {
    return openDb().then((db) => {
      return new Promise((resolve, reject) => {
        if (typeof projectId !== "string") {
          reject(new Error("Project id must be a string"));
          return;
        }

        const transaction = db.transaction(projectStoreName, "readwrite");
        const store = transaction.objectStore(projectStoreName);

        transaction.oncomplete = () => {
          resolve();
        };

        transaction.onerror = () => {
          reject(transaction.error || new Error("Failed to delete project"));
        };

        store.delete(projectId);
      });
    });
  }

  function init(app) {
    app.ports.loadProjects.subscribe(() => {
      getAllProjects()
        .then((projects) => {
          app.ports.projectsLoaded.send(projects);
        })
        .catch((error) => {
          reportStorageError(app, error);
        });
    });

    app.ports.saveProject.subscribe((project) => {
      putProject(project).catch((error) => {
        reportStorageError(app, error);
      });
    });

    app.ports.deleteProject.subscribe((projectId) => {
      deleteProject(projectId).catch((error) => {
        reportStorageError(app, error);
      });
    });
  }

  window.TodoGraphProjectStorage = {
    init,
  };
})();
