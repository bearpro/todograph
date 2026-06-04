import { Module } from "@nestjs/common";

import { DatabaseModule } from "./database/database.module";
import { ProjectsModule } from "./projects/projects.module";

@Module({
  imports: [DatabaseModule, ProjectsModule]
})
export class AppModule {}
