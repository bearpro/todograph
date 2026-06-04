import { Module } from "@nestjs/common";

import { DatabaseModule } from "../database/database.module";
import { ProjectsController } from "./projects.controller";
import { ProjectUpdatesGateway } from "./project-updates.gateway";
import { ProjectsService } from "./projects.service";

@Module({
  imports: [DatabaseModule],
  controllers: [ProjectsController],
  providers: [ProjectUpdatesGateway, ProjectsService],
  exports: [ProjectUpdatesGateway, ProjectsService]
})
export class ProjectsModule {}
