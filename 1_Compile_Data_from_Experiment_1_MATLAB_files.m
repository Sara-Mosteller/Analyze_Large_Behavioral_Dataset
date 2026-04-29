%% Overview

%This script loops through the raw data file for each participant, extracts
% the stimulus properties and response variables from the p, prefs and stim
%structures, and writes these to a single csv file with one column per variable.

%Section labels list the variables to be formatted within the section.
% Within each data file, the code: 
%Converts each cell array (itemLocs, itemColors) into a matrix with a single row per cell. 
%Scales all cells to the same size by inputting 'NA' for missing values. 
%Reshapes matrices to be one row per trial. 
%Generates a vector that repeats scalar variables (such as participant ID) 
%to make a column in the dataset. 

%After downloading the data, create a main directory with Data_Experiment_1
%that contains both the shared MATLAB and CSV participant files. 

% Before running, add the cellflat.m function to the directory, found at: 
% https://www.mathworks.com/matlabcentral/fileexchange/50502-flatten-nested-cell-arrays

%Navigate to the main data directory and add the Data_Experiment_1 folder to the path

%% Loop through the participant files in the Data_experiment_1/IndividualFiles folder

baseDir = 'Data_experiment_1/IndividualFiles/';
%rejectPattern = 'REJECT';
titlePattern = '*_ColorK_9colors.mat'; % ** allows recursive search in subfolders

% Get all matching files
allFiles = dir(fullfile(baseDir, titlePattern));

% Remove folder entries from the list (keep files only)
isFolder = [allFiles.isdir];
allFiles(isFolder) = [];
% Remove the reject files from the list-these are the last  two rows in the
% allFiles structure. 
allFiles = allFiles(1:135);


% Loop through the files - this loop carries to near the end of the script
for k = 1 : length(allFiles)
    fullFileName = fullfile(allFiles(k).folder, allFiles(k).name);
    fprintf('Processing: %s\n', fullFileName);
    
    load(fullFileName);
    % Perform operations on the file below


    %% Probe Location
    
    probeLoc = reshape(stim.probeLoc, 60*9, 2);
    
    clearvars -except allFiles p prefs stim probeLoc
    
    %% Item Locations
    
    celllist = {'itemLocs'};
    
    out = {};
    for i = 1:length(celllist)
        if iscell(stim.(celllist{i}))
            out = [out cellflat(stim.(celllist{i}))];
        else
            out = [out stim.(celllist{i})];
        end 
    
    
    
    out = transpose(out);
    
    
    
    %Loop through the rows of the transposed structure
    % and make every cell the same size. 
    
    %Find and store the maximum cell dimensions
    
       [a(:,1),a(:,2)]=cellfun(@size,out,'UniformOutput',false);
        a_mat = cell2mat(a);
        max_dim = max(a_mat);
    
    %Find and store the minimum cell dimensions
    
        [a(:,1),a(:,2)]=cellfun(@size,out,'UniformOutput',false);
        a_mat = cell2mat(a);
        min_dim = min(a_mat);
    
    
    end
    
    %Pad all cells in the array with NA values to reach the max number of
    %columns, since all cells have the same number of rows.
    
    out2 = {};
    
    for i = 1:length(out)
        blank_cell = NaN(max_dim);
        blank_cell(1:2, 1:length(out{i})) = out{i};
        out2{i} = blank_cell;
        out2{i} = out2{i}.'; %Flatten these cells so that the 2x8 dimension goes to 1x16
        out2{i} = out2{i}(:)';
        
    end
    
    out2 = transpose(out2);
    itemLocs = cell2mat(out2);
    
    clearvars -except allFiles p prefs stim probeLoc itemLocs
    
    %% 
    
    
    %Item colors
    
    celllist = {'itemColors'};
    
    out = {};
    for i = 1:length(celllist)
        if iscell(stim.(celllist{i}))
            out = [out cellflat(stim.(celllist{i}))];
        else
            out = [out stim.(celllist{i})];
        end 
    end
    
    
    out = transpose(out);
    
    
    %Find and store the maximum cell dimensions
    
        [a(:,1)]=cellfun(@size,out,'UniformOutput',false);
        a_mat = cell2mat(a);
        max_dim = max(a_mat);
    
    %Find and store the minimum cell dimensions
    
        [a(:,1)]=cellfun(@size,out,'UniformOutput',false);
        a_mat = cell2mat(a);
        min_dim = min(a_mat);
    
    %Pad all cells in the array with NA values to reach the max number of
    %columns, since all cells have the same number of rows.
    
    out2 = {};
    
    for i = 1:length(out)
        blank_cell = NaN(max_dim);
        blank_cell(1, 1:length(out{i})) = out{i};
        out2{i} = blank_cell;
        out2{i} = out2{i}.'; %Flatten these cells so that the 2x8 dimension goes to 1x16
        
    end
    
    itemColors = transpose(cell2mat(out2));
    clearvars -except allFiles p prefs stim probeLoc itemLocs itemColors
    
    
    %% Remaining matrix variables
    
    matrixlist = {'setSize', 'change', 'response', 'rt', 'presentedColor', 'probeColor'};
    out = {};
    for i = 1:length(matrixlist)
        out{i} = reshape(stim.(matrixlist{i}), 60*9, []);
    end
    
    setSize = out{1};
    change = out{2};
    response = out{3};
    response(response == 90) = 0;
    response(response == 191) = 1;
    rt = out{4};
    presentedColor = out{5};
    probeColor = out{6};
    
    clearvars -except allFiles p prefs stim probeLoc itemLocs itemColors setSize change response rt presentedColor probeColor
    
    %% Create the remaining variables
    
    %Create the remaining variables
    ID = p.subNum * ones(540, 1);
    block =  transpose(repelem(1:9, 60)); 
    trial_num_overall = transpose(1:540);
    trial_num_within_block = transpose(repmat(1:60, 1, 9)); 
    stim_duration = prefs.stimulusDuration * ones(540, 1);
    ITI = prefs.ITI * ones(540, 1);
    retention_interval = prefs.retentionInterval * ones(540, 1);
    break_duration = prefs.breakLength * ones(540, 1);
    stim_size = prefs.stimSize * ones(540, 1);
    fixation_size = prefs.fixationSize * ones(540, 1);
    min_distance = prefs.minDist * ones(540, 1);
    
    %% Combine into one dataset and write to a CSV file
    
    partdata = [ID, trial_num_overall, block, trial_num_within_block, setSize, change,... 
        response, rt, stim_size, stim_duration, fixation_size, ITI, retention_interval,...
        break_duration, min_distance, presentedColor, probeColor, probeLoc, itemColors, itemLocs]; 
    
    writematrix(partdata, 'raw_trial_data.csv', 'WriteMode', 'append')

    clearvars -except allFiles

end