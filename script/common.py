benchmarks = {
    "ClockBoundFormalModels" : "ClockBoundFormalModels/ClockBound",
    "S3IndexBrickManagerPModels" : "S3IndexBrickManagerPModels",
    "Kermit2PCModel": "Kermit2PCModel",
    "MemoryDBCRRConflictResolutionModel": "MemoryDBCRRConflictResolutionModel",
    "ChainReplication": "ChainReplication"
}

cases = [
"ClockBoundFormalModels",
# "S3IndexBrickManagerPModels",
# "Kermit2PCModel",
# "MemoryDBCRRConflictResolutionModel",
# "ChainReplication"
]

prefix = "/Users/zhezzhou/workspace/amazon_ws/src/TestExamples-PTestGeneration/"

client_name_to_tc = {
    "ChainReplication": {
        "GClientHead": "Syn___synMode_1",
        "GClientTail": "Syn___synMode_2",
        "GClientMid": "Syn___synMode_3",
        "RandomClient": "ChainRep"
    },
    "MemoryDBCRRConflictResolutionModel": {
        "GClientDelEx": "Syn___mode_1",
        "GClientDelNotEx": "Syn___mode_2",
        "RandomClient": "tc2Region2Command"
    },
    "S3IndexBrickManagerPModels" : {
        "RandomClient": "tcSingleClient_GetPutListDelete_WithSplitsMerges_OnRandomPerkle",
        "ReadAfterUpdate": "Syn___mode_1",
        "NoAdj": "Syn___mode_2",
    },
    "Kermit2PCModel" : {
        "GClientSameKey": "Syn___mode_1",
        "GClientDiffKey": "Syn___mode_2",
        "RandomClient": "tcSingleClientSingleRouterNoFailures",
    },
    "ClockBoundFormalModels" : {
        "GClientSame": "Syn___mode_1",
        "GClientDiff": "Syn___mode_2",
        "RandomClient": "tcClockBoundCheckWithSingleClient",
    }
}
