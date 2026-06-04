import "reflect-metadata";

import { NestFactory } from "@nestjs/core";
import { NestExpressApplication } from "@nestjs/platform-express";
import express from "express";
import { existsSync } from "node:fs";
import { join, resolve } from "node:path";

import { AppModule } from "./app.module";
import { ProjectUpdatesGateway } from "./projects/project-updates.gateway";

function configureStaticApp(app: NestExpressApplication): void {
  const frontendDist =
    process.env.FRONTEND_DIST || resolve(__dirname, "../../app/dist");
  const indexHtml = join(frontendDist, "index.html");

  if (!existsSync(indexHtml)) {
    return;
  }

  const expressApp = app.getHttpAdapter().getInstance();

  expressApp.use(express.static(frontendDist, { index: false }));
  expressApp.get(/^\/(?!api(?:\/|$)|ws(?:\/|$)).*/, (_req, res) => {
    res.sendFile(indexHtml);
  });
}

async function bootstrap(): Promise<void> {
  const app = await NestFactory.create<NestExpressApplication>(AppModule, {
    bodyParser: false
  });

  app.use(express.json({ limit: process.env.JSON_BODY_LIMIT || "10mb" }));
  app.setGlobalPrefix("api");

  configureStaticApp(app);

  const updatesGateway = app.get(ProjectUpdatesGateway);
  updatesGateway.attach(app.getHttpServer());

  const port = Number(process.env.PORT || 8080);
  await app.listen(port, "0.0.0.0");
}

void bootstrap();
