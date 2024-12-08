-- Create the matchups table
IF NOT EXISTS (SELECT * FROM sys.tables WHERE name = 'matchups')
BEGIN
    CREATE TABLE matchups (
        MatchID VARCHAR(255) PRIMARY KEY,
        Court VARCHAR(255),
        Team1_Player1 VARCHAR(255),
        Team1_Player2 VARCHAR(255),
        Team2_Player1 VARCHAR(255),
        Team2_Player2 VARCHAR(255),
        Team1_Score INT,
        Team2_Score INT,
        SysTime DATETIME2
    );
END;

-- Create the player_results table
IF NOT EXISTS (SELECT * FROM sys.tables WHERE name = 'player_results')
BEGIN
    CREATE TABLE player_results (
        Player VARCHAR(255),
        MatchID VARCHAR(255),
        Result VARCHAR(10), -- 'Win' or 'Loss'
        Points_Scored INT,
        Points_Against INT,
        Point_Difference INT,
        PRIMARY KEY (Player, MatchID)
    );
END;

-- Create the player_summary table
IF NOT EXISTS (SELECT * FROM sys.tables WHERE name = 'player_summary')
BEGIN
    CREATE TABLE player_summary (
        Player VARCHAR(255) PRIMARY KEY,
        Matches_Played INT,
        Matches_Won INT,
        Win_Percentage FLOAT,
        Points_For INT,
        Points_Against INT,
        Point_Differential INT,
        Rank INT
    );
END;
