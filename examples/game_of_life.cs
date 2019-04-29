// Recommend not to run with Interpreter. It's too slow.

using System;

public class GameOfLife {
  static void DrawGrid(bool[] grid, int grid_width, int grid_height) {
    Console.WriteLine("\x1b\x5b\x31\x3b\x31\x48\x1b\x5b\x32\x4a-----");
    for (int y = 1; y < grid_height; y++) {
      for (int x = 1; x < grid_width; x++) {
        Console.Write(grid[x + grid_width * y] ? "#" : " ");
      }
      Console.WriteLine("");
    }
  }
  
  static void UpdateGrid(bool[] grid, bool[] copy_grid, int grid_width, int grid_height) {
    for (int y = 1; y < grid_height - 1; y++) {
      for (int x = 1; x < grid_width - 1; x++) {
        int total_cells = 0;
       
        total_cells += grid[(x - 1) + grid_width * (y - 1)] ? 1 : 0;
        total_cells += grid[(x - 1) + grid_width * (y    )] ? 1 : 0;
        total_cells += grid[(x - 1) + grid_width * (y + 1)] ? 1 : 0;
        total_cells += grid[(x    ) + grid_width * (y - 1)] ? 1 : 0;
        total_cells += grid[(x    ) + grid_width * (y + 1)] ? 1 : 0;
        total_cells += grid[(x + 1) + grid_width * (y - 1)] ? 1 : 0;
        total_cells += grid[(x + 1) + grid_width * (y    )] ? 1 : 0;
        total_cells += grid[(x + 1) + grid_width * (y + 1)] ? 1 : 0;
    
        if (grid[x + grid_width * y]) {
          if (total_cells == 0 || total_cells == 1) {
            copy_grid[x + y * grid_width] = false; 
          } else if (total_cells == 2 || total_cells == 3) {
            copy_grid[x + y * grid_width] = true; 
          } else if (4 <= total_cells && total_cells <= 8) {
            copy_grid[x + y * grid_width] = false; 
          } else {
            copy_grid[x + y * grid_width] = false;
          }
        } else { 
          if (total_cells == 3) {
            copy_grid[x + y * grid_width] = true; 
          } else {
            copy_grid[x + y * grid_width] = false; 
          }
        }
      }
    }
    
    for (int i = 0; i < grid.Length; i++) {
      grid[i] = copy_grid[i];
    }
  }

  static void FillWithGlider(bool[] grid, int grid_width) {
    grid[25 + grid_width * 1] = true;
    grid[23 + grid_width * 2] = true;
    grid[25 + grid_width * 2] = true;
    grid[13 + grid_width * 3] = true;
    grid[14 + grid_width * 3] = true;
    grid[21 + grid_width * 3] = true;
    grid[22 + grid_width * 3] = true;
    grid[35 + grid_width * 3] = true;
    grid[36 + grid_width * 3] = true;
    grid[12 + grid_width * 4] = true;
    grid[16 + grid_width * 4] = true;
    grid[21 + grid_width * 4] = true;
    grid[22 + grid_width * 4] = true;
    grid[35 + grid_width * 4] = true;
    grid[36 + grid_width * 4] = true;
    grid[1 + grid_width * 5] = true;
    grid[2 + grid_width * 5] = true;
    grid[11 + grid_width * 5] = true;
    grid[17 + grid_width * 5] = true;
    grid[21 + grid_width * 5] = true;
    grid[22 + grid_width * 5] = true;
    grid[1 + grid_width * 6] = true;
    grid[2 + grid_width * 6] = true;
    grid[11 + grid_width * 6] = true;
    grid[15 + grid_width * 6] = true;
    grid[17 + grid_width * 6] = true;
    grid[18 + grid_width * 6] = true;
    grid[23 + grid_width * 6] = true;
    grid[25 + grid_width * 6] = true;
    grid[11 + grid_width * 7] = true;
    grid[17 + grid_width * 7] = true;
    grid[25 + grid_width * 7] = true;
    grid[12 + grid_width * 8] = true;
    grid[16 + grid_width * 8] = true;
    grid[13 + grid_width * 9] = true;
    grid[14 + grid_width * 9] = true;
  }

  public static void Main() {
    int grid_width = 53, grid_height = 43;
    bool[] grid      = new bool[grid_width * grid_height];
    bool[] copy_grid = new bool[grid_width * grid_height];
    
    FillWithGlider(grid, grid_width);
 
    while (true) {
      DrawGrid(grid, grid_width, grid_height);
      UpdateGrid(grid, copy_grid, grid_width, grid_height);
    }
  }
}
