import {
  BadRequestException,
  HttpException,
  NotFoundException
} from "@nestjs/common";

import { ProjectsController } from "./projects.controller";
import { ProjectsService } from "./projects.service";

function responseOf(error: unknown): unknown {
  if (error instanceof HttpException) {
    return error.getResponse();
  }

  return null;
}

describe("ProjectsController", () => {
  const fixedNow = 1_800_000_000_000;
  let controller: ProjectsController;
  let service: {
    checkUpdates: jest.Mock;
    getProject: jest.Mock;
    saveProject: jest.Mock;
  };

  beforeEach(() => {
    jest.spyOn(Date, "now").mockReturnValue(fixedNow);

    service = {
      checkUpdates: jest.fn(),
      getProject: jest.fn(),
      saveProject: jest.fn()
    };

    controller = new ProjectsController(service as unknown as ProjectsService);
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  it("rejects invalid updatedAt values", async () => {
    await expect(
      controller.saveProject("project-1", { updatedAt: "bad", payload: {} })
    ).rejects.toBeInstanceOf(BadRequestException);

    await expect(
      controller.saveProject("project-1", { updatedAt: "bad", payload: {} })
    ).rejects.toMatchObject({
      response: {
        status: "rejected",
        reason: "invalid_updated_at"
      }
    });
  });

  it("rejects updatedAt values more than one second in the future", async () => {
    await expect(
      controller.saveProject("project-1", {
        updatedAt: fixedNow + 1001,
        payload: {}
      })
    ).rejects.toMatchObject({
      response: {
        status: "rejected",
        reason: "updated_at_from_future"
      }
    });
  });

  it("returns accepted save responses", async () => {
    service.saveProject.mockResolvedValue({
      status: "accepted",
      projectId: "project-1",
      updatedAt: fixedNow
    });

    await expect(
      controller.saveProject("project-1", {
        updatedAt: fixedNow,
        payload: { id: "project-1" }
      })
    ).resolves.toEqual({
      status: "accepted",
      projectId: "project-1",
      updatedAt: fixedNow
    });
  });

  it("maps stale saves to HTTP 409", async () => {
    service.saveProject.mockResolvedValue({
      status: "rejected",
      reason: "stale_update",
      serverUpdatedAt: fixedNow
    });

    try {
      await controller.saveProject("project-1", {
        updatedAt: fixedNow - 1,
        payload: {}
      });
    } catch (error) {
      expect(error).toBeInstanceOf(HttpException);
      expect((error as HttpException).getStatus()).toBe(409);
      expect(responseOf(error)).toEqual({
        status: "rejected",
        reason: "stale_update",
        serverUpdatedAt: fixedNow
      });
      return;
    }

    throw new Error("Expected controller.saveProject to reject");
  });

  it("returns 404 for missing projects", async () => {
    service.getProject.mockResolvedValue(null);

    await expect(controller.getProject("missing")).rejects.toBeInstanceOf(
      NotFoundException
    );
    await expect(controller.getProjectVersion("missing")).rejects.toBeInstanceOf(
      NotFoundException
    );
  });
});
