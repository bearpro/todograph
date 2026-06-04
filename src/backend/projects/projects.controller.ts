import {
  BadRequestException,
  Body,
  Controller,
  Get,
  HttpException,
  HttpStatus,
  NotFoundException,
  Param,
  Post,
  Put
} from "@nestjs/common";

import {
  CheckProjectUpdateInput,
  ProjectsService
} from "./projects.service";

type SaveProjectBody = {
  updatedAt?: unknown;
  payload?: unknown;
};

type CheckUpdatesBody = Array<{
  projectId?: unknown;
  updatedAt?: unknown;
}>;

@Controller("projects")
export class ProjectsController {
  constructor(private readonly projectsService: ProjectsService) {}

  @Post("check-updates")
  async checkUpdates(@Body() body: unknown) {
    const inputs = this.parseCheckUpdatesBody(body);

    return this.projectsService.checkUpdates(inputs);
  }

  @Get(":projectId/version")
  async getProjectVersion(@Param("projectId") projectId: string) {
    const project = await this.projectsService.getProject(projectId);

    if (!project) {
      throw new NotFoundException();
    }

    return {
      projectId,
      updatedAt: project.updatedAt
    };
  }

  @Get(":projectId")
  async getProject(@Param("projectId") projectId: string) {
    const project = await this.projectsService.getProject(projectId);

    if (!project) {
      throw new NotFoundException();
    }

    return {
      projectId,
      updatedAt: project.updatedAt,
      payload: project.payload
    };
  }

  @Put(":projectId")
  async saveProject(
    @Param("projectId") projectId: string,
    @Body() body: SaveProjectBody
  ) {
    const updatedAt = this.parseUpdatedAt(body?.updatedAt);
    this.rejectFutureUpdatedAt(updatedAt);

    const result = await this.projectsService.saveProject(
      projectId,
      updatedAt,
      body?.payload
    );

    if (result.status === "accepted") {
      return result;
    }

    throw new HttpException(result, HttpStatus.CONFLICT);
  }

  private parseUpdatedAt(value: unknown): number {
    if (
      typeof value !== "number" ||
      !Number.isFinite(value) ||
      !Number.isInteger(value) ||
      value < 0
    ) {
      throw new BadRequestException({
        status: "rejected",
        reason: "invalid_updated_at"
      });
    }

    return value;
  }

  private rejectFutureUpdatedAt(updatedAt: number): void {
    if (updatedAt > Date.now() + 1000) {
      throw new BadRequestException({
        status: "rejected",
        reason: "updated_at_from_future"
      });
    }
  }

  private parseCheckUpdatesBody(body: unknown): CheckProjectUpdateInput[] {
    if (!Array.isArray(body)) {
      return [];
    }

    return (body as CheckUpdatesBody)
      .filter(
        (input) =>
          input &&
          typeof input.projectId === "string" &&
          typeof input.updatedAt === "number" &&
          Number.isFinite(input.updatedAt)
      )
      .map((input) => ({
        projectId: input.projectId as string,
        updatedAt: input.updatedAt as number
      }));
  }
}
