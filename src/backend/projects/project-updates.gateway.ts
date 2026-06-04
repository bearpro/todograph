import { Injectable, OnApplicationShutdown } from "@nestjs/common";
import { Server as HttpServer } from "node:http";
import { WebSocket, WebSocketServer } from "ws";

type ClientMessage =
  | { type: "subscribeProject"; projectId: string }
  | { type: "unsubscribeProject"; projectId: string };

@Injectable()
export class ProjectUpdatesGateway implements OnApplicationShutdown {
  private server: WebSocketServer | null = null;
  private readonly projectSubscribers = new Map<string, Set<WebSocket>>();
  private readonly socketSubscriptions = new Map<WebSocket, Set<string>>();

  attach(httpServer: HttpServer): void {
    if (this.server) {
      return;
    }

    this.server = new WebSocketServer({ server: httpServer, path: "/ws" });

    this.server.on("connection", (socket) => {
      this.socketSubscriptions.set(socket, new Set());

      socket.on("message", (message) => {
        this.handleMessage(socket, message.toString());
      });

      socket.on("close", () => {
        this.removeSocket(socket);
      });

      socket.on("error", () => {
        this.removeSocket(socket);
      });
    });
  }

  broadcastProjectUpdated(projectId: string, updatedAt: number): void {
    const subscribers = this.projectSubscribers.get(projectId);

    if (!subscribers) {
      return;
    }

    const message = JSON.stringify({
      type: "projectUpdated",
      projectId,
      updatedAt
    });

    for (const socket of subscribers) {
      if (socket.readyState === WebSocket.OPEN) {
        socket.send(message);
      } else {
        this.removeSocket(socket);
      }
    }
  }

  async onApplicationShutdown(): Promise<void> {
    if (!this.server) {
      return;
    }

    await new Promise<void>((resolve) => {
      this.server?.close(() => resolve());
    });
  }

  private handleMessage(socket: WebSocket, rawMessage: string): void {
    const message = this.parseMessage(rawMessage);

    if (!message) {
      return;
    }

    if (message.type === "subscribeProject") {
      this.subscribe(socket, message.projectId);
      return;
    }

    this.unsubscribe(socket, message.projectId);
  }

  private parseMessage(rawMessage: string): ClientMessage | null {
    let decoded: unknown;

    try {
      decoded = JSON.parse(rawMessage);
    } catch {
      return null;
    }

    if (!decoded || typeof decoded !== "object") {
      return null;
    }

    const message = decoded as Partial<ClientMessage>;

    if (
      (message.type === "subscribeProject" ||
        message.type === "unsubscribeProject") &&
      typeof message.projectId === "string" &&
      message.projectId.length > 0
    ) {
      return message as ClientMessage;
    }

    return null;
  }

  private subscribe(socket: WebSocket, projectId: string): void {
    let subscribers = this.projectSubscribers.get(projectId);

    if (!subscribers) {
      subscribers = new Set();
      this.projectSubscribers.set(projectId, subscribers);
    }

    subscribers.add(socket);

    const subscriptions = this.socketSubscriptions.get(socket) || new Set();
    subscriptions.add(projectId);
    this.socketSubscriptions.set(socket, subscriptions);
  }

  private unsubscribe(socket: WebSocket, projectId: string): void {
    const subscribers = this.projectSubscribers.get(projectId);
    subscribers?.delete(socket);

    if (subscribers && subscribers.size === 0) {
      this.projectSubscribers.delete(projectId);
    }

    const subscriptions = this.socketSubscriptions.get(socket);
    subscriptions?.delete(projectId);
  }

  private removeSocket(socket: WebSocket): void {
    const subscriptions = this.socketSubscriptions.get(socket);

    if (subscriptions) {
      for (const projectId of subscriptions) {
        const subscribers = this.projectSubscribers.get(projectId);
        subscribers?.delete(socket);

        if (subscribers && subscribers.size === 0) {
          this.projectSubscribers.delete(projectId);
        }
      }
    }

    this.socketSubscriptions.delete(socket);
  }
}
