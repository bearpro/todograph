const cacheName = "todo-graph-app-v1";

const appShellAssets = [
  "/",
  "/index.html",
  "/main.js",
  "/app.css",
  "/vendor/bootstrap-5.3.8.min.css",
  "/manifest.webmanifest",
  "/favicon.ico",
  "/favicon.svg",
  "/favicon-16x16.png",
  "/favicon-32x32.png",
  "/favicon-48x48.png",
  "/apple-touch-icon.png",
  "/icon-192x192.png",
  "/icon-512x512.png",
  "/icons/bootstrap/github.svg",
  "/icons/fluent/add_circle_20_regular.svg",
  "/icons/fluent/add_square_20_regular.svg",
  "/icons/fluent/arrow_clockwise_20_regular.svg",
  "/icons/fluent/cloud_checkmark_20_regular.svg",
  "/icons/fluent/cloud_off_20_regular.svg",
  "/icons/fluent/cloud_sync_20_regular.svg",
  "/icons/fluent/copy_20_regular.svg",
  "/icons/fluent/delete_20_regular.svg",
  "/icons/fluent/edit_20_regular.svg",
  "/icons/fluent/link_20_regular.svg",
  "/icons/fluent/link_dismiss_20_regular.svg",
  "/icons/fluent/play_20_regular.svg",
  "/icons/fluent/stop_20_regular.svg",
  "/ports/project-storage.js",
  "/ports/project-sync.js",
];

self.addEventListener("install", (event) => {
  event.waitUntil(
    caches.open(cacheName).then((cache) => cache.addAll(appShellAssets))
  );
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches
      .keys()
      .then((keys) =>
        Promise.all(keys.filter((key) => key !== cacheName).map((key) => caches.delete(key)))
      )
      .then(() => self.clients.claim())
  );
});

self.addEventListener("fetch", (event) => {
  const request = event.request;

  if (request.method !== "GET") {
    return;
  }

  const url = new URL(request.url);

  if (url.origin !== self.location.origin) {
    return;
  }

  if (url.pathname.startsWith("/api/") || url.pathname.startsWith("/ws/")) {
    return;
  }

  if (request.mode === "navigate") {
    event.respondWith(networkFirstNavigation(request));
    return;
  }

  event.respondWith(cacheFirst(request));
});

async function networkFirstNavigation(request) {
  const cache = await caches.open(cacheName);

  try {
    const response = await fetch(request);

    if (response.ok) {
      await cache.put("/index.html", response.clone());
    }

    return response;
  } catch (_error) {
    return (await cache.match("/index.html")) || Response.error();
  }
}

async function cacheFirst(request) {
  const cached = await caches.match(request);

  if (cached) {
    return cached;
  }

  const response = await fetch(request);

  if (response.ok) {
    const cache = await caches.open(cacheName);
    await cache.put(request, response.clone());
  }

  return response;
}
