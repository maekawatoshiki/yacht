using System;

public class GameOfLife {
  // static final int grid_height = 20;
  // static final int grid_width  = 20;
  // static boolean   grid[]      = null;
  // static boolean   copy_grid[] = null;
  //
  // static void fill_with_random() {
  //   for (int i = 0; i < grid_width * grid_height; i++) {
  //     grid[i] = Math.random() < 0.5 ? true : false;
  //   }
  // }
  //
  static void DrawGrid(bool[] grid, int grid_width, int grid_height) {
    Console.WriteLine("-----");
    for (int y = 1; y < grid_height; y++) {
      for (int x = 1; x < grid_width; x++) {
        Console.Write(grid[x + grid_height * y] ? "#" : " ");
      }
      Console.WriteLine("");
    }
  }
  
  static void UpdateGrid(bool[] grid, bool[] copy_grid, int grid_width, int grid_height) {
    for (int y = 1; y < grid_height - 1; y++) {
      for (int x = 1; x < grid_width - 1; x++) {
        int total_cells = 0;
       
        total_cells += grid[(x - 1) + grid_height * (y - 1)] ? 1 : 0;
        total_cells += grid[(x - 1) + grid_height * (y    )] ? 1 : 0;
        total_cells += grid[(x - 1) + grid_height * (y + 1)] ? 1 : 0;
        total_cells += grid[(x    ) + grid_height * (y - 1)] ? 1 : 0;
        total_cells += grid[(x    ) + grid_height * (y + 1)] ? 1 : 0;
        total_cells += grid[(x + 1) + grid_height * (y - 1)] ? 1 : 0;
        total_cells += grid[(x + 1) + grid_height * (y    )] ? 1 : 0;
        total_cells += grid[(x + 1) + grid_height * (y + 1)] ? 1 : 0;
    
        if (grid[x + grid_height * y]) {
          if (total_cells == 0 || total_cells == 1) {
            copy_grid[x + y * grid_height] = false; 
          } else if (total_cells == 2 || total_cells == 3) {
            copy_grid[x + y * grid_height] = true; 
          } else if (4 <= total_cells && total_cells <= 8) {
            copy_grid[x + y * grid_height] = false; 
          } else {
            copy_grid[x + y * grid_height] = false;
          }
        } else { 
          if (total_cells == 3) {
            copy_grid[x + y * grid_height] = true; 
          } else {
            copy_grid[x + y * grid_height] = false; 
          }
        }
      }
    }
    
    for (int i = 0; i < grid_width * grid_height; i++) {
      grid[i] = copy_grid[i];
    }
  }

  static void FillWithRandom(bool[] grid) {
    for (int i = 0; i < grid.Length; i++) {
      if (i % 10 == 0) grid[i] = true; else grid[i] = false;
    }
  }

  public static void Main() {
    int grid_width = 40, grid_height = 20;
    bool[] grid      = new bool[grid_width * grid_height];
    bool[] copy_grid = new bool[grid_width * grid_height];

    FillWithRandom(grid);
  
    for (int i = 0; i < 50; i++) {
      UpdateGrid(grid, copy_grid, grid_width, grid_height);
      DrawGrid(grid, grid_width, grid_height);
    }
  }
}
