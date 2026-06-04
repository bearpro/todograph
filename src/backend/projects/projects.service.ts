import { Inject, Injectable } from "@nestjs/common";
import { Collection, MongoServerError } from "mongodb";

import { PROJECTS_COLLECTION } from "../database/database.module";
import { ProjectUpdatesGateway } from "./project-updates.gateway";
import { StoredProject } from "./stored-project";

export type SaveProjectResult =
  | { status: "accepted"; projectId: string; updatedAt: number }
  | {
      status: "rejected";
      reason: "stale_update";
      serverUpdatedAt: number;
    };

export type CheckProjectUpdateInput = {
  projectId: string;
  updatedAt: number;
};

export type CheckProjectUpdateResult =
  | {
      projectId: string;
      exists: true;
      hasUpdate: boolean;
      serverUpdatedAt: number;
    }
  | {
      projectId: string;
      exists: false;
      hasUpdate: false;
    };

@Injectable()
export class ProjectsService {
  constructor(
    @Inject(PROJECTS_COLLECTION)
    private readonly projects: Collection<StoredProject>,
    private readonly updatesGateway: ProjectUpdatesGateway
  ) {}

  async getProject(projectId: string): Promise<StoredProject | null> {
    return this.projects.findOne({ _id: projectId });
  }

  async saveProject(
    projectId: string,
    updatedAt: number,
    payload: unknown
  ): Promise<SaveProjectResult> {
    for (let attempt = 0; attempt < 3; attempt += 1) {
      const updateResult = await this.projects.updateOne(
        { _id: projectId, updatedAt: { $lt: updatedAt } },
        { $set: { updatedAt, payload } }
      );

      if (updateResult.modifiedCount === 1) {
        this.updatesGateway.broadcastProjectUpdated(projectId, updatedAt);
        return { status: "accepted", projectId, updatedAt };
      }

      try {
        await this.projects.insertOne({ _id: projectId, updatedAt, payload });
        this.updatesGateway.broadcastProjectUpdated(projectId, updatedAt);
        return { status: "accepted", projectId, updatedAt };
      } catch (error) {
        if (!this.isDuplicateKeyError(error)) {
          throw error;
        }
      }

      const current = await this.getProject(projectId);

      if (!current) {
        continue;
      }

      if (current.updatedAt >= updatedAt) {
        return {
          status: "rejected",
          reason: "stale_update",
          serverUpdatedAt: current.updatedAt
        };
      }
    }

    const current = await this.getProject(projectId);

    if (current && current.updatedAt >= updatedAt) {
      return {
        status: "rejected",
        reason: "stale_update",
        serverUpdatedAt: current.updatedAt
      };
    }

    return this.saveProject(projectId, updatedAt, payload);
  }

  async checkUpdates(
    inputs: CheckProjectUpdateInput[]
  ): Promise<CheckProjectUpdateResult[]> {
    const projectIds = inputs.map((input) => input.projectId);
    const storedProjects = await this.projects
      .find({ _id: { $in: projectIds } })
      .toArray();
    const byId = new Map(
      storedProjects.map((project) => [project._id, project])
    );

    return inputs.map((input) => {
      const stored = byId.get(input.projectId);

      if (!stored) {
        return {
          projectId: input.projectId,
          exists: false,
          hasUpdate: false
        };
      }

      return {
        projectId: input.projectId,
        exists: true,
        hasUpdate: stored.updatedAt > input.updatedAt,
        serverUpdatedAt: stored.updatedAt
      };
    });
  }

  private isDuplicateKeyError(error: unknown): boolean {
    return error instanceof MongoServerError && error.code === 11000;
  }
}
