FROM node:24-bookworm-slim AS frontend-build

WORKDIR /workspace/src/app

RUN apt-get update \
  && apt-get install --yes --no-install-recommends ca-certificates \
  && rm -rf /var/lib/apt/lists/*

RUN npm install --global elm@0.19.1-6

COPY src/app/elm.json ./
COPY src/app/ ./

RUN mkdir -p ./dist \
  && rm -rf ./dist/* \
  && elm make Main.elm --optimize --output=dist/main.js \
  && cp -R ./static/. ./dist/


FROM node:24-bookworm-slim AS backend-build

ENV PNPM_HOME=/pnpm
ENV PATH=$PNPM_HOME:$PATH

RUN corepack enable && corepack prepare pnpm@11.4.0 --activate

WORKDIR /workspace/src/backend

COPY src/backend/package.json src/backend/pnpm-lock.yaml src/backend/pnpm-workspace.yaml ./
RUN pnpm install --frozen-lockfile

COPY src/backend/ ./
RUN pnpm build


FROM node:24-bookworm-slim AS backend-prod-deps

ENV PNPM_HOME=/pnpm
ENV PATH=$PNPM_HOME:$PATH

RUN corepack enable && corepack prepare pnpm@11.4.0 --activate

WORKDIR /workspace/src/backend

COPY src/backend/package.json src/backend/pnpm-lock.yaml src/backend/pnpm-workspace.yaml ./
RUN pnpm install --frozen-lockfile --prod


FROM node:24-bookworm-slim AS runtime

ENV NODE_ENV=production
ENV PORT=8080
ENV FRONTEND_DIST=/app/frontend

WORKDIR /app/backend

COPY --from=backend-build /workspace/src/backend/dist ./dist
COPY --from=backend-prod-deps /workspace/src/backend/node_modules ./node_modules
COPY --from=backend-prod-deps /workspace/src/backend/package.json ./package.json
COPY --from=frontend-build /workspace/src/app/dist /app/frontend

EXPOSE 8080

CMD ["node", "dist/main.js"]
