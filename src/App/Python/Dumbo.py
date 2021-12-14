def flash(x, y, grid, flashes):
    # print('flashing ', x, y)
    grid[x][y] = 0
    flashes += 1
    for x2 in range(x - 1, x + 2):
        for y2 in range(y - 1, y + 2):
            # print('checking ', x2, y2)
            if -1 < x2 < len(grid) and -1 < y2 < len(grid[x]):
                if not(x == x2 and y == y2) and grid[x2][y2] != 0:
                    grid[x2][y2] += 1

                    if grid[x2][y2] >= 10:
                        (grid, flashes) = flash(x2, y2, grid, flashes)

    return (grid, flashes)

if __name__ == '__main__':
#     input = '''11111
# 19991
# 19191
# 19991
# 11111'''
#     input = '''5483143223
# 2745854711
# 5264556173
# 6141336146
# 6357385478
# 4167524645
# 2176841721
# 6882881134
# 4846848554
# 5283751526'''
    input = '''4438624262
6263251864
2618812434
2134264565
1815131247
2612457325
8585767584
7217134556
2825456563
8248473584
'''

    grid = []
    lines = input.splitlines()
    for l in lines:
        g = list(l)
        g2 = list(map(lambda s: int(s), g))
        grid.append(g2)

    xMax = len(grid)
    yMax = len(grid[0])
    flashes = 0
    steps = 0
    superflash = False

    print(grid)

    while not superflash:
        steps += 1

        # Step start
        for x in range(xMax):
            for y in range(yMax):
                grid[x][y] += 1

        # print(grid)

        # Flash check
        for x in range(xMax):
            for y in range(yMax):
                if grid[x][y] >= 10:
                    (grid, flashes) = flash(x, y, grid, flashes)

        zerocount = 0
        for x in range(xMax):
            for y in range(yMax):
                if grid[x][y] == 0:
                    zerocount += 1

        if (zerocount == xMax * yMax) or (steps > 200000):
            superflash = True

    print(grid)
    print(flashes)
    print(steps)

