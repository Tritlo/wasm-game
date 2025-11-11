// Preload script - Secure bridge between main and renderer processes
const { contextBridge, ipcRenderer } = require('electron');

// Expose Steamworks API to renderer process
contextBridge.exposeInMainWorld('steamworks', {
  // Steam API initialization status
  isInitialized: () => {
    return ipcRenderer.invoke('steamworks:isInitialized');
  },

  // Achievements
  getAchievement: (achievementName) => {
    return ipcRenderer.invoke('steamworks:getAchievement', achievementName);
  },
  setAchievement: (achievementName) => {
    return ipcRenderer.invoke('steamworks:setAchievement', achievementName);
  },
  clearAchievement: (achievementName) => {
    return ipcRenderer.invoke('steamworks:clearAchievement', achievementName);
  },

  // Stats
  getStat: (statName) => {
    return ipcRenderer.invoke('steamworks:getStat', statName);
  },
  setStat: (statName, value) => {
    return ipcRenderer.invoke('steamworks:setStat', statName, value);
  },
  storeStats: () => {
    return ipcRenderer.invoke('steamworks:storeStats');
  },

  // Leaderboards
  findLeaderboard: (leaderboardName) => {
    return ipcRenderer.invoke('steamworks:findLeaderboard', leaderboardName);
  },
  uploadLeaderboardScore: (handle, score, details) => {
    return ipcRenderer.invoke('steamworks:uploadLeaderboardScore', handle, score, details);
  },
  downloadLeaderboardEntries: (handle, requestType, start, end) => {
    return ipcRenderer.invoke('steamworks:downloadLeaderboardEntries', handle, requestType, start, end);
  },

  // Cloud saves
  fileExists: (fileName) => {
    return ipcRenderer.invoke('steamworks:fileExists', fileName);
  },
  fileWrite: (fileName, data) => {
    return ipcRenderer.invoke('steamworks:fileWrite', fileName, data);
  },
  fileRead: (fileName) => {
    return ipcRenderer.invoke('steamworks:fileRead', fileName);
  },
  fileDelete: (fileName) => {
    return ipcRenderer.invoke('steamworks:fileDelete', fileName);
  },

  // Friends
  getFriendCount: () => {
    return ipcRenderer.invoke('steamworks:getFriendCount');
  },
  getFriendByIndex: (index) => {
    return ipcRenderer.invoke('steamworks:getFriendByIndex', index);
  },

  // User info
  getSteamId: () => {
    return ipcRenderer.invoke('steamworks:getSteamId');
  },
  getPersonaName: () => {
    return ipcRenderer.invoke('steamworks:getPersonaName');
  },

  // Events
  on: (eventName, callback) => {
    ipcRenderer.on(`steamworks:${eventName}`, (event, ...args) => callback(...args));
  },
  off: (eventName, callback) => {
    ipcRenderer.removeListener(`steamworks:${eventName}`, callback);
  },
});
