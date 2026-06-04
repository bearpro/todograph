module.exports = {
  moduleFileExtensions: ["js", "json", "ts"],
  rootDir: ".",
  testRegex: ".*\\.spec\\.ts$",
  transform: {
    "^.+\\.(t|j)s$": [
      "ts-jest",
      {
        "tsconfig": "tsconfig.spec.json"
      }
    ]
  },
  collectCoverageFrom: [
    "*.(t|j)s",
    "database/**/*.(t|j)s",
    "projects/**/*.(t|j)s"
  ],
  testEnvironment: "node"
};
