class SokobanState:

    def __init__(self, board_layout, player_pos):
        self.board = [list(row) for row in board_layout]
        self.player_pos = tuple(player_pos) # (row, col)
        self.rows = len(self.board)
        self.cols = len(self.board[0])
        self.goals = self._find_goals()

    def _find_goals(self):
        goals = []
        for r in range(self.rows):
            for c in range(self.cols):
                if self.board[r][c] == '.':
                    goals.append((r, c))
        return set(goals)

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

    # check if its a wall
    def _is_wall(self, r: int, c: int):
        if not self.is_valid_pos(r, c):
            return True
        return self.board[r][c] == '#'

    # check if there is any deadlock
    #like:
    #      ##
    #      $#
    #       #
    def deadlock(self):
        for r in range(self.rows):
            for c in range(self.cols):
                if self.board[r][c] != '$':
                    continue
                if (r, c) in self.goals:
                    continue

                parede_cima = self._is_wall(r - 1, c)
                parede_baixo = self._is_wall(r + 1, c)
                parede_esq = self._is_wall(r, c - 1)
                parede_dir = self._is_wall(r, c + 1)

                if (parede_cima and parede_esq) or \
                   (parede_cima and parede_dir) or \
                   (parede_baixo and parede_esq) or \
                   (parede_baixo and parede_dir):
                    return True
        return False
    
    def clone(self):
        return SokobanState([list(row) for row in self.board], self.player_pos)