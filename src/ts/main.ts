import { ElmType } from "../Main.elm";

export function init(Elm: ElmType) {
  const node = document.getElementById("main");
  if (node == null) {
    document.write(
      "Error initializing application. This might be caused by a browser extension."
    );
    return;
  }

  const app = Elm.Main.init({
    node: node,
    flags: {
      hasShowDirectoryPicker: "showDirectoryPicker" in window,
    },
  });

  var cachedDirHandle: FileSystemDirectoryHandle | null = null;

  async function getDirHandle(): Promise<FileSystemDirectoryHandle | null> {
    if (!("showDirectoryPicker" in window)) return null;

    try {
      return await window.showDirectoryPicker();
    } catch (error) {
      return null;
    }
  }

  async function getCachedDirHandle(): Promise<FileSystemDirectoryHandle | null> {
    if (!cachedDirHandle) cachedDirHandle = await getDirHandle();
    return cachedDirHandle;
  }

  app.ports.resetPickedDirectory.subscribe(function () {
    cachedDirHandle = null;
    app.ports.pickedDirectoryReset.send(null);
  });

  app.ports.saveFile.subscribe(async function ({ number, content }) {
    const dirHandle = await getCachedDirHandle();
    if (!dirHandle) {
      app.ports.saveFailed.send(number);
      return;
    }

    try {
      let filename = "frame" + number.toString().padStart(4, "0") + ".svg";
      const fileHandle = await dirHandle.getFileHandle(filename, {
        create: true,
      });
      const writable = await fileHandle.createWritable();
      await writable.write(content);
      await writable.close();
      app.ports.saveSucceeded.send(number);
    } catch (error) {
      app.ports.saveFailed.send(number);
    }
  });
}
