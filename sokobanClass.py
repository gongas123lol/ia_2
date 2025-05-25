class SokobanState:
    def __init__(self, board_layout, player_pos):
        self.board = [list(row) for row in board_layout] # Make a mutable copy
        self.player_pos = tuple(player_pos) # (row, col)
        self.rows = len(self.board)
        self.cols = len(self.board[0])
        self.goals = self._find_goals() # Pre-compute goal positions

    def _find_goals(self):
        goals = []
        for r in range(self.rows):
            for c in range(self.cols):
                if self.board[r][c] == '.': # Goal square
                    goals.append((r, c))
        return set(goals) # Use a set for efficient lookup

    def get_char(self, r, c):
        if (r, c) == self.player_pos:
            return 'P'
        return self.board[r][c]

    def display(self):
        for r in range(self.rows):
            row_str = ""
            for c in range(self.cols):
                if (r, c) == self.player_pos:
                    row_str += 'P'
                else:
                    row_str += self.board[r][c]
            print(row_str)
        print("-" * self.cols)

    def is_valid_pos(self, r, c):
        return 0 <= r < self.rows and 0 <= c < self.cols

    def __eq__(self, other):
        if not isinstance(other, SokobanState):
            return NotImplemented
        return self.player_pos == other.player_pos and self.board == other.board

#    def __hash__(self):
 #       # Hashing for set/dict keys, convert board to a frozenset of tuples for immutability
  #      board_tuple = tuple(tuple(row) for row in self.board)
   #     return hash((self.player_pos, board_tuple))

    def clone(self):
        return SokobanState([list(row) for row in self.board], self.player_pos)

