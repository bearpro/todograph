module.exports = {
  moduleFileExtensions: ["js", "json", "ts"],
  rootDir: ".",
  testMatch: ["<rootDir>/test/**/*.spec.ts"],
  transform: {
    "^.+\\.(t|j)s$": [
      "ts-jest",
      {
        "tsconfig": "tsconfig.spec.json"
      }
    ]
  },
  collectCoverageFrom: [
    "src/**/*.(t|j)s"
  ],
  testEnvironment: "node"
};
