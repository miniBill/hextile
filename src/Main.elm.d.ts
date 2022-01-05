export const Elm: ElmType;

export type ElmType = {
  Main: {
    init: (config: {
      node: HTMLElement;
      flags: {
        hasShowDirectoryPicker: boolean;
      };
    }) => MainInstance;
  };
};

export type MainInstance = {
  ports: {
    resetPickedDirectory: {
      subscribe(callback: () => void): void;
    };
    pickedDirectoryReset: {
      send(dummy: null): void;
    };
    saveFile: {
      subscribe(callback: ({ number: number, content: string }) => void): void;
    };
    saveSucceeded: {
      send(number: number): void;
    };
    saveFailed: {
      send(number: number): void;
    };
  };
};
