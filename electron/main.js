const { app, BrowserWindow, ipcMain, session } = require('electron');
const path = require('path');
const fs = require('fs');
const steamworksConfig = require('./steamworks-config');

let mainWindow;
let steamworksClient = null;
let steamInitialized = false;

// Initialize Steamworks
function initSteamworks() {
  try {
    // Only try to initialize on Windows/Linux (Steamworks doesn't support macOS)
    if (process.platform !== 'win32' && process.platform !== 'linux') {
      console.log('Steamworks not supported on this platform');
      return false;
    }

    let steamworksModule;
    try {
      steamworksModule = require('steamworks.js');
    } catch (err) {
      console.warn('Could not load steamworks.js:', err.message);
      console.warn('Steamworks features will be disabled');
      return false;
    }

    // Initialize Steam API with App ID
    try {
      steamworksClient = steamworksModule.init(steamworksConfig.appId);
      console.log('Steam API initialized successfully');
      steamInitialized = true;

      // Enable Steam Overlay
      steamworksModule.electronEnableSteamOverlay();

      return true;
    } catch (err) {
      console.warn('Steam API initialization failed - Steam may not be running:', err.message);
      return false;
    }
  } catch (error) {
    console.error('Error initializing Steamworks:', error);
    return false;
  }
}

// Initialize Steamworks before app is ready
initSteamworks();

function createWindow() {
  // Create the main window
  mainWindow = new BrowserWindow({
    width: 1280,
    height: 720,
    backgroundColor: '#000000', // Set background color to prevent red frame on Linux
    frame: false, // Remove window frame to eliminate red border on Linux/X11
    transparent: false,
    hasShadow: false, // Remove shadow which may include border on Steam Deck
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      nodeIntegration: false,
      contextIsolation: true,
      enableRemoteModule: false,
      // Enable gamepad API
      webSecurity: true,
    },
    icon: path.join(__dirname, '..', 'build', 'icon.png'),
    autoHideMenuBar: true,
    show: true, // Show immediately
  });

  // Remove menu bar completely
  mainWindow.setMenu(null);

  // Additional settings to remove border on Steam Deck
  mainWindow.setHasShadow(false);

  // Detect Steam Deck (Gamescope compositor adds borders)
  // Check for Steam Deck environment or Gamescope compositor
  const isSteamDeck = process.env.STEAM_DECK === '1' ||
                      process.env.XDG_CURRENT_DESKTOP === 'gamescope' ||
                      (process.env.DISPLAY === ':0' && process.env.XDG_SESSION_TYPE === 'wayland');

  // Ensure window is fully borderless after creation (for Steam Deck)
  mainWindow.once('ready-to-show', () => {
    if (mainWindow && !mainWindow.isDestroyed()) {
      mainWindow.setHasShadow(false);
      // On Steam Deck, use fullscreen to eliminate compositor border
      // Gamescope adds focus borders that can only be removed with fullscreen
      if (isSteamDeck) {
        mainWindow.setFullScreen(true);
      } else {
        // Force refresh window properties on other platforms
        const bounds = mainWindow.getBounds();
        mainWindow.setBounds(bounds);
      }
    }
  });

  // Set up window event handlers
  mainWindow.on('closed', () => {
    mainWindow = null;
  });

  // Ensure window can receive focus for gamepad input
  mainWindow.on('focus', () => {
    // Re-activate gamepad polling when window gains focus
    if (mainWindow && !mainWindow.isDestroyed()) {
      mainWindow.webContents.executeJavaScript(`
        if (navigator.getGamepads) {
          // Trigger gamepad access on focus
          navigator.getGamepads();
        }
      `).catch(err => console.error('Error activating gamepads on focus:', err));
    }
  });

  // Load the game HTML file
  const htmlPath = path.join(__dirname, '..', 'index.html');
  mainWindow.loadFile(htmlPath);

  // Enable gamepad support - Electron requires user interaction to activate gamepads
  mainWindow.webContents.on('did-finish-load', () => {
    // Inject code to enable gamepad API
    // In Electron, gamepads need user interaction to activate, so we'll trigger on any user input
    mainWindow.webContents.executeJavaScript(`
      // Function to activate gamepads through user interaction
      function activateGamepads() {
        if (navigator.getGamepads) {
          // Try to access gamepads - this requires user interaction in Electron
          try {
            const gamepads = navigator.getGamepads();
            console.log('Gamepads accessed:', gamepads.length);
            for (let i = 0; i < gamepads.length; i++) {
              if (gamepads[i]) {
                console.log('Gamepad found:', i, gamepads[i].id, gamepads[i].mapping);
              }
            }
          } catch (e) {
            console.error('Error accessing gamepads:', e);
          }
        }
      }

      // Activate gamepads on any user interaction (click, keypress, etc.)
      document.addEventListener('click', activateGamepads, { once: true });
      document.addEventListener('keydown', activateGamepads, { once: true });
      document.addEventListener('pointerdown', activateGamepads, { once: true });

      // Also try to activate immediately (might work if window has focus)
      setTimeout(activateGamepads, 100);

      // Poll gamepads continuously after activation
      setInterval(() => {
        try {
          const gamepads = navigator.getGamepads();
          // Access gamepads to keep them active
          for (let i = 0; i < gamepads.length; i++) {
            if (gamepads[i]) {
              gamepads[i].buttons;
              gamepads[i].axes;
            }
          }
        } catch (e) {
          // Silently handle errors
        }
      }, 50);

      // Listen for gamepad connection events
      window.addEventListener('gamepadconnected', (e) => {
        console.log('Gamepad connected:', e.gamepad.id, e.gamepad.mapping);
        activateGamepads(); // Re-activate when new gamepad connects
      });

      window.addEventListener('gamepaddisconnected', (e) => {
        console.log('Gamepad disconnected:', e.gamepad.id);
      });
    `).catch(err => console.error('Error enabling gamepad:', err));
  });

  // Open DevTools in development
  if (process.env.NODE_ENV === 'development') {
    mainWindow.webContents.openDevTools();
  }
}

// Steamworks IPC Handlers
function setupSteamworksHandlers() {
  // Initialization
  ipcMain.handle('steamworks:isInitialized', () => {
    return steamInitialized;
  });

  // Achievements
  ipcMain.handle('steamworks:getAchievement', async (event, achievementName) => {
    if (!steamInitialized || !steamworksClient) return false;
    try {
      return steamworksClient.achievements.getAchievement(achievementName);
    } catch (err) {
      console.error('Error getting achievement:', err);
      return false;
    }
  });

  ipcMain.handle('steamworks:setAchievement', async (event, achievementName) => {
    if (!steamInitialized || !steamworksClient) return false;
    try {
      steamworksClient.achievements.activate(achievementName);
      console.log('Achievement unlocked:', achievementName);
      return true;
    } catch (err) {
      console.error('Error setting achievement:', err);
      return false;
    }
  });

  ipcMain.handle('steamworks:clearAchievement', async (event, achievementName) => {
    if (!steamInitialized || !steamworksClient) return false;
    try {
      steamworksClient.achievements.clear(achievementName);
      console.log('Achievement cleared:', achievementName);
      return true;
    } catch (err) {
      console.error('Error clearing achievement:', err);
      return false;
    }
  });

  // Stats
  ipcMain.handle('steamworks:getStat', async (event, statName) => {
    if (!steamInitialized || !steamworksClient) return 0;
    try {
      return steamworksClient.stats.getStatInt(statName) || steamworksClient.stats.getStatFloat(statName) || 0;
    } catch (err) {
      console.error('Error getting stat:', err);
      return 0;
    }
  });

  ipcMain.handle('steamworks:setStat', async (event, statName, value) => {
    if (!steamInitialized || !steamworksClient) return false;
    try {
      if (typeof value === 'number') {
        if (Number.isInteger(value)) {
          steamworksClient.stats.setStatInt(statName, value);
        } else {
          steamworksClient.stats.setStatFloat(statName, value);
        }
      }
      return true;
    } catch (err) {
      console.error('Error setting stat:', err);
      return false;
    }
  });

  ipcMain.handle('steamworks:storeStats', async () => {
    if (!steamInitialized || !steamworksClient) return false;
    try {
      steamworksClient.stats.storeStats();
      return true;
    } catch (err) {
      console.error('Error storing stats:', err);
      return false;
    }
  });

  // Leaderboards
  ipcMain.handle('steamworks:findLeaderboard', async (event, leaderboardName) => {
    if (!steamInitialized || !steamworksClient) return null;
    try {
      return new Promise((resolve, reject) => {
        steamworksClient.leaderboards.findOrCreateLeaderboard(leaderboardName, 'Descending', 'Numeric', (result) => {
          if (result.result === 'OK') {
            resolve(result.leaderboard);
          } else {
            reject(new Error('Failed to find or create leaderboard'));
          }
        });
      });
    } catch (err) {
      console.error('Error finding leaderboard:', err);
      return null;
    }
  });

  ipcMain.handle('steamworks:uploadLeaderboardScore', async (event, handle, score, details) => {
    if (!steamInitialized || !steamworksClient) return false;
    try {
      return new Promise((resolve) => {
        steamworksClient.leaderboards.uploadScore(handle, 'KeepBest', score, details, (result) => {
          resolve(result.result === 'OK');
        });
      });
    } catch (err) {
      console.error('Error uploading leaderboard score:', err);
      return false;
    }
  });

  ipcMain.handle('steamworks:downloadLeaderboardEntries', async (event, handle, requestType, start, end) => {
    if (!steamInitialized || !steamworksClient) return [];
    try {
      return new Promise((resolve) => {
        steamworksClient.leaderboards.downloadEntries(handle, requestType, start, end, (result) => {
          if (result.result === 'OK') {
            resolve(result.entries || []);
          } else {
            resolve([]);
          }
        });
      });
    } catch (err) {
      console.error('Error downloading leaderboard entries:', err);
      return [];
    }
  });

  // Cloud saves
  ipcMain.handle('steamworks:fileExists', async (event, fileName) => {
    if (!steamInitialized || !steamworksClient) return false;
    try {
      return steamworksClient.remoteStorage.fileExists(fileName);
    } catch (err) {
      console.error('Error checking file existence:', err);
      return false;
    }
  });

  ipcMain.handle('steamworks:fileWrite', async (event, fileName, data) => {
    if (!steamInitialized || !steamworksClient) return false;
    try {
      const buffer = Buffer.from(data, 'utf8');
      steamworksClient.remoteStorage.fileWrite(fileName, buffer);
      return true;
    } catch (err) {
      console.error('Error writing file:', err);
      return false;
    }
  });

  ipcMain.handle('steamworks:fileRead', async (event, fileName) => {
    if (!steamInitialized || !steamworksClient) return null;
    try {
      const buffer = steamworksClient.remoteStorage.fileRead(fileName);
      return buffer.toString('utf8');
    } catch (err) {
      console.error('Error reading file:', err);
      return null;
    }
  });

  ipcMain.handle('steamworks:fileDelete', async (event, fileName) => {
    if (!steamInitialized || !steamworksClient) return false;
    try {
      steamworksClient.remoteStorage.fileDelete(fileName);
      return true;
    } catch (err) {
      console.error('Error deleting file:', err);
      return false;
    }
  });

  // Friends
  ipcMain.handle('steamworks:getFriendCount', async () => {
    if (!steamInitialized || !steamworksClient) return 0;
    try {
      return steamworksClient.friends.getFriendCount();
    } catch (err) {
      console.error('Error getting friend count:', err);
      return 0;
    }
  });

  ipcMain.handle('steamworks:getFriendByIndex', async (event, index) => {
    if (!steamInitialized || !steamworksClient) return null;
    try {
      return steamworksClient.friends.getFriendByIndex(index);
    } catch (err) {
      console.error('Error getting friend by index:', err);
      return null;
    }
  });

  // User info
  ipcMain.handle('steamworks:getSteamId', async () => {
    if (!steamInitialized || !steamworksClient) return null;
    try {
      return steamworksClient.localplayer.getSteamId();
    } catch (err) {
      console.error('Error getting Steam ID:', err);
      return null;
    }
  });

  ipcMain.handle('steamworks:getPersonaName', async () => {
    if (!steamInitialized || !steamworksClient) return null;
    try {
      return steamworksClient.localplayer.getName();
    } catch (err) {
      console.error('Error getting persona name:', err);
      return null;
    }
  });
}
app.commandLine.appendSwitch('ozone-platform', 'x11');
app.commandLine.appendSwitch('disable-features', 'Vulkan,WebGPU');
app.commandLine.appendSwitch('enable-features', 'UseOzonePlatform');

app.whenReady().then(() => {
  // Enable gamepad API through session permissions
  session.defaultSession.setPermissionRequestHandler((webContents, permission, callback) => {
    if (permission === 'gamepad') {
      callback(true); // Allow gamepad access
    } else {
      callback(false);
    }
  });

  setupSteamworksHandlers();
  createWindow();

  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) {
      createWindow();
    }
  });
});

app.on('window-all-closed', () => {
  // On Linux/Steam Deck, always quit immediately
  if (process.platform !== 'darwin') {
    // Ensure all windows are destroyed before exiting
    const windows = BrowserWindow.getAllWindows();
    windows.forEach(win => {
      if (!win.isDestroyed()) {
        win.destroy();
      }
    });
    // Force exit immediately
    app.exit(0);
  }
});

app.on('will-quit', () => {
  // Cleanup Steamworks if initialized
  if (steamInitialized && steamworksClient) {
    try {
      // Check if shutdown method exists before calling
      if (typeof steamworksClient.shutdown === 'function') {
        steamworksClient.shutdown();
      }
    } catch (err) {
      console.error('Error shutting down Steamworks:', err);
    }
  }
});
