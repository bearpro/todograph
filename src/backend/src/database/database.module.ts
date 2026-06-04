import { Inject, Module, OnApplicationShutdown } from "@nestjs/common";
import { Collection, MongoClient } from "mongodb";

import { StoredProject } from "../projects/stored-project";

export const MONGO_CLIENT = Symbol("MONGO_CLIENT");
export const PROJECTS_COLLECTION = Symbol("PROJECTS_COLLECTION");

class MongoClientShutdown implements OnApplicationShutdown {
  constructor(@Inject(MONGO_CLIENT) private readonly client: MongoClient) {}

  async onApplicationShutdown(): Promise<void> {
    await this.client.close();
  }
}

@Module({
  providers: [
    {
      provide: MONGO_CLIENT,
      useFactory: async (): Promise<MongoClient> => {
        const uri =
          process.env.MONGODB_URI || "mongodb://127.0.0.1:27017/todo-graph";
        const client = new MongoClient(uri, {
          serverSelectionTimeoutMS: Number(
            process.env.MONGODB_SERVER_SELECTION_TIMEOUT_MS || 5000
          )
        });

        await client.connect();

        return client;
      }
    },
    {
      provide: PROJECTS_COLLECTION,
      useFactory: async (
        client: MongoClient
      ): Promise<Collection<StoredProject>> => {
        const dbName = process.env.MONGODB_DB || "todo-graph";
        const collection = client.db(dbName).collection<StoredProject>("projects");

        await collection.createIndex({ updatedAt: 1 });

        return collection;
      },
      inject: [MONGO_CLIENT]
    },
    MongoClientShutdown
  ],
  exports: [PROJECTS_COLLECTION]
})
export class DatabaseModule {}
