function MakeExperimentFromStimuliFileFlexQuestions( ...
    subject, ...
    pathWhereOutputSubjectDirShouldBeCreated, ... 
    sentenceFile, ...
    numBlocks )

    sharedDir = GetSharedDir();
    stimulusMetaHelpDir = fullfile(sharedDir, 'stimulus_metadata_helpers');
    addpath(genpath(stimulusMetaHelpDir));
    utilDir = fullfile(sharedDir, 'utility');
    addpath(genpath(utilDir));
    
    %%%%%%%%%% Experiment Generation Script - KRNS Default %%%%%%%%%%
    %
    % This code takes a stimulus input file and outputs experiment 
    % code that can be used with PsychToolBox to present stimuli.
    %
    % Input: 
    %   subject                                     
    %       The name of the subject, e.g. 'G'
    %   pathWhereOutputSubjectDirShouldBeCreated
    %       The base output path not including the subject specific part of the
    %       path
    %       e.g. /usr1/meg/krns4/behav_data
    %   sentenceFile
    %       e.g. <ExpName>_stimuli.txt
    %       Tab-delimited file with 4 columns
    %       <Full Sentence w/PoS markings> <Question> <Answer> <ConditionCode>
    %   numBlocks
    %       The number of blocks to present in the experiment
    %
    % Output: sentenceBlock.mat
    %   Containts 'experiment' data structure with 9 variables:
    %   .stimulus               Which word is presented at the time
    %   .trigger                Trigger code to send to parallel port
    %   .duration               Total duration this item should be presented
    %   .timestamp              Total time stamp within block
    %
    %%%%%%%%%% Port/Trigger ordering: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %   Example
    %   1 = Prompt text that is displayed in block 1 only
    %   2 = Onset of first word of sentence (SENTENCE MARKER)
    %   3 = Onset of each individual word thereafter (WORD MARKER)
    %   4 = Question Markers
    %   5 = Onset of the first word of a passage (PASSAGE MARKER)
    %
    %   5 0 3 0 3 0 3 0 3 0 2 0 3 0 3 0 3 0 4 0 253 0 
    %   is a 2 sentence passage where the first sentence has
    %   5 words and the second sentence has 4 words. 
    %   The passage has a Q/A after it, which has the 3rd permutation (in
    %   lexigraphical order) of the answer choices
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    numRepetitionsForPauseFmt = 'numRepetitionsPause%d';
    numSecondsPauseFmt = 'numSecondsPause%d';
    questionAnswerFmt = 'answer%d';
    
    [   passages, configuration ] = ...
        ReadTaggedTabDelimitedFile( sentenceFile );

    promptLines = {};
    while( true )
        promptLineKey = sprintf('promptLine%d', length(promptLines) + 1);
        if (isfield(configuration, promptLineKey))
            promptLines{end+1} = configuration.(promptLineKey); %#ok<AGROW>
        else
            break;
        end
    end
    
    prompt = strjoin(promptLines, '\n');
    
    % how many different pause settings do we have
    numPauses = 1;
    while (true)
        if (~isfield(passages{1}, sprintf(numRepetitionsForPauseFmt, numPauses)))
            numPauses = numPauses - 1;
            break;
        end
        
        numPauses = numPauses + 1;
    end
    
    numAnswers = 1;
    while (true)
        if (~isfield(passages{1}, sprintf(questionAnswerFmt, numAnswers)))
            numAnswers = numAnswers - 1;
            break;
        end
        
        numAnswers = numAnswers + 1;
    end
    
    % Seeds the random number generator so that every time you rerun this
    % script for this participant, it will generate the same output code.
    s = RandStream('mt19937ar','Seed',int8(subject));
    RandStream.setGlobalStream(s);
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%  CREATE STIMULI BLOCKS %%%%%%%%%%%%%%%%%%%%%%%
    %
    % Clear any prior and then create empty cell arrays for these three
    % variables.
    %
    % 1st pass: figure out the passage order, and where the questions will
    % go
    
    % for every passage-pause setting that is repeated more than numBlocks
    % times, assign floor(numReps / numBlocks) repetitions to each block so
    % they are evenly distributed.  Then choose random blocks for the
    % remainders
    
    % set up the directory for output
    subjectDir = fullfile(pathWhereOutputSubjectDirShouldBeCreated, subject);
    if ~exist(subjectDir, 'dir')
        mkdir(subjectDir);
    end
    
    blockAssignments = cell(numBlocks, 1);
    for indexBlock = 1:numBlocks
        blockAssignments{indexBlock} = nan(0, 2);
    end

    for indexPassage = 1:length(passages)
        for indexPause = 1:numPauses
            repField = sprintf(numRepetitionsForPauseFmt, indexPause);
            numRepetitions = passages{indexPassage}.(repField);
            
            numRepsAllBlocks = floor(cast(numRepetitions,'double')/numBlocks);
            for indexBlock = 1:numBlocks
                for indexRep = 1:numRepsAllBlocks
                    blockAssignments{indexBlock}(size(blockAssignments{indexBlock},1) + 1, :) = ...
                        [indexPassage, indexPause];
                end
            end
            
            numExtra = numRepetitions - (numRepsAllBlocks * numBlocks);
            if (numExtra > 0)
                extraBlocks = randperm(numBlocks, numExtra);
                for indexExtra = 1:length(extraBlocks)
                    indexBlock = extraBlocks(indexExtra);
                    blockAssignments{indexBlock}(size(blockAssignments{indexBlock},1) + 1, :) = ...
                        [indexPassage, indexPause];
                end
            end
        end
                
    end
    
    % now randomly reorder the passages within the blocks
    % attempting to never have back-to-back presentations
    % within or across blocks
    maxIter = 1000;
    bestOrder = [];
    bestScore = inf;
    for indexIter = 1:maxIter
        
        currentOrder = cell(numBlocks, 1);
        currentScore = 0;
        indexPassageLast = NaN;
        for indexBlock = 1:numBlocks
            
            blockOrder = randperm(size(blockAssignments{indexBlock},1));
            currentOrder{indexBlock} = blockOrder;
            
            for indexInBlock = 1:length(blockOrder)
                
                indexPassageCurrent = ...
                    blockAssignments{indexBlock}(blockOrder(indexInBlock), 1);
                
                if (indexPassageLast == indexPassageCurrent)
                    currentScore = currentScore + 1;
                end
                
                indexPassageLast = indexPassageCurrent;
                
            end
                    
        end
        
        if (currentScore < bestScore)
            bestOrder = currentOrder;
            bestScore = currentScore;
        end
            
        if (bestScore == 0)
            fprintf('Found order with no repeats after %d tries\n', indexIter);
            break;
        end
            
    end
    
    if (bestScore ~= 0)
        fprintf('Unable to find order with no repeats after %d tries, %d sentences appear consecutively\n', maxIter, bestScore);
    end
    
    for indexBlock = 1:numBlocks
        
        % apply the order we found above
        blockAssignments{indexBlock} = ...
            blockAssignments{indexBlock}(bestOrder{indexBlock}, :);
        
    end
    
    answerOrder = cell(numBlocks, 1);
    blockStreamSizes = zeros(numBlocks);
    
    for indexBlock = 1:numBlocks
        
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Take a random sample weighted by the number of occurrences of a
        % passage
        %
        
        answerOrder{indexBlock} = zeros(size(blockAssignments{indexBlock}, 1), numAnswers);
        
        % calculate the number of questions in this block
        numQuestions = configuration.questionRate * size(blockAssignments{indexBlock}, 1);
        
        % randomly choose whether to round up based on remainder
        % so expected value is questionRate
        remainder = numQuestions - floor(numQuestions);
        numQuestions = floor(numQuestions);
        if (rand(1, 1) < remainder)
            numQuestions = numQuestions + 1;
        end
        
        % count the number of each passage
        % also count the tokens
        uniquePassageCount = containers.Map('KeyType', 'double', 'ValueType', 'double');
        sizeTokens = 0;
        sizeBetweenTokenBlanks = 0;
        for indexAssignment = 1:size(blockAssignments{indexBlock}, 1)
            idPassage = blockAssignments{indexBlock}(indexAssignment, 1);
            sizeTokens = sizeTokens + length(passages{idPassage}.stimulus);
            sizeBetweenTokenBlanks = sizeBetweenTokenBlanks + length(passages{idPassage}.stimulus) - 1;
            if (isKey(uniquePassageCount, idPassage))
                countPassage = uniquePassageCount(idPassage);
            else
                countPassage = 0;
            end
            uniquePassageCount(idPassage) = countPassage + 1;
        end
        
        uniquePassageIds = cell2mat(uniquePassageCount.keys());
        uniquePassageCounts = cell2mat(uniquePassageCount.values());
        
        % take a weighted random sample
        questionPassageIndices = ReservoirSample(uniquePassageCounts, numQuestions);
        
        % choose which presentation of the sampled passages
        % will get a question
        sizeQuestions = 0;
        for indexQuestion = 1:length(questionPassageIndices)
            passageId = uniquePassageIds(indexQuestion);
            indexChosenPresentation = randperm(uniquePassageCounts(indexQuestion), 1);
            allPresentations = ...
                find(blockAssignments{indexBlock}(:, 1) == passageId);
            indexAssignment = allPresentations(indexChosenPresentation);
        
            numAnswersCurrent = 0;
            for indexAnswer = 1:numAnswers
                answerField = sprintf(questionAnswerFmt, indexAnswer);
                if (~isempty(passages{passageId}.(answerField)))
                    numAnswersCurrent = numAnswersCurrent + 1;
                else
                    break;
                end
            end
            answerOrder{indexBlock}(indexAssignment, 1:numAnswersCurrent) = randperm(numAnswersCurrent);

            questionWordCount = length(passages{passageId}.question);
            
            % words plus in-betweens
            sizeQuestions = ...
                sizeQuestions + questionWordCount + questionWordCount - 1;
        end
        
        % now we have enough information to allocate storage
        sizeInstructions = 2; % instruction prompt and blank
        if (indexBlock ~= 1)
            sizeInstructions = 0;
        end
        
        % subtract 1 from tokens for each sentence = subtract num sentences
        sizeEndOfPassageRest = size(blockAssignments{indexBlock}, 1) - 1;
        sizeEndOfBlock = 1;
        
        blockStreamSizes(indexBlock) = ...
            sizeInstructions + ...
            sizeTokens + ...
            sizeBetweenTokenBlanks + ...
            sizeQuestions + ...
            sizeEndOfPassageRest + ...
            sizeEndOfBlock;
        
    end
    
    % now write out the experiment
    for indexBlock = 1:numBlocks
        
        clear('blockOutput');
        
        % final experiment structure that is interpretable by
        % psych Toolbox
        blockOutput.stimulus = cell(1, blockStreamSizes(indexBlock));
        blockOutput.trigger = zeros(1, blockStreamSizes(indexBlock));
        blockOutput.duration = zeros(1, blockStreamSizes(indexBlock));
        blockOutput.timestamp = zeros(1, blockStreamSizes(indexBlock));
        
        indexBlockOutput = 1;
        cumulativeTimestamp = 0;
        
        % If it is the very first block, include the prompt instructions
        % but skip otherwise.
        if indexBlock == 1
            
            [ blockOutput, indexBlockOutput, cumulativeTimestamp ] = ...
                WriteOutput( ...
                    blockOutput, ...
                    indexBlockOutput, ...
                    cumulativeTimestamp, ...
                    prompt, ... % stimulusText, ...
                    configuration.promptTrigger, ... % trigger, ...
                    configuration.timePerPrompt ); 
            
            [ blockOutput, indexBlockOutput, cumulativeTimestamp ] = ...
                WriteOutput( ...
                    blockOutput, ...
                    indexBlockOutput, ...
                    cumulativeTimestamp, ...
                    '+', ... % stimulusText, ...
                    0, ... % trigger, ...
                    configuration.timePerFix ); % duration
                
        end
        
        % Construct sentences
        % example - 60 sentences with 2 reps/block, = 120
        for indexInBlock = 1:size(blockAssignments{indexBlock})

            passage = passages{blockAssignments{indexBlock}(indexInBlock, 1)};
            indexPause = blockAssignments{indexBlock}(indexInBlock, 2);
            pauseField = sprintf(numSecondsPauseFmt, indexPause);
            pauseTime = passage.(pauseField);
            
            %%%%%%%% BUILD THE SENTENCE %%%%%%%%
            % 

            % Iterate through word positions
            % subtract 1 because the final word in each sentence is '.'
            for indexWordPosition = 1:length(passage.stimulus)
                
                % For words after the first word in the sentence, write the
                % blank before writing the word
                if (indexWordPosition > 1)
                    
                    if (passage.stimulus{indexWordPosition}.trigger ...
                            == configuration.pauseTrigger ...
                        && pauseTime >= 0)
                        % the current trigger is special, e.g. start of
                        % sentence; use the pause time from the columns
                        iti = pauseTime;
                    else
                        % take the iti from the word before this
                        iti = passage.stimulus{indexWordPosition-1}.iti;
                    end
                    
                    [ blockOutput, indexBlockOutput, cumulativeTimestamp ] = ...
                        WriteOutput( ...
                            blockOutput, ...
                            indexBlockOutput, ...
                            cumulativeTimestamp, ...
                            ' ', ... % stimulusText, ...
                            0, ... % trigger, ...
                            iti); 
                    
                end
                
                word = passage.stimulus{indexWordPosition}.word;
                trigger = passage.stimulus{indexWordPosition}.trigger;
                duration = passage.stimulus{indexWordPosition}.duration;
                
                [ blockOutput, indexBlockOutput, cumulativeTimestamp ] = ...
                    WriteOutput( ...
                        blockOutput, ...
                        indexBlockOutput, ...
                        cumulativeTimestamp, ...
                        word, ... % stimulusText, ...
                        trigger, ... % trigger, ...
                        duration ); 
                
            end
            
            % if there is no question should be this duration
            % if there is a question it gets cut in half
            finalRestPeriod = configuration.itiStimuli;
               
            if (answerOrder{indexBlock}(indexInBlock, 1) ~= 0)
                
                finalRestPeriod = configuration.itiStimuliPostQuestion;
                questionWords = passage.question;
                currentAnswerOrder = answerOrder{indexBlock}(indexInBlock, :);
                currentAnswerOrder = currentAnswerOrder(currentAnswerOrder ~= 0);
                answerChoiceText = '';
                for indexAnswer = 1:length(currentAnswerOrder)
                    if (indexAnswer > 1)
                       answerChoiceText = sprintf('%s%s', answerChoiceText, configuration.answerSeparator);
                    end
                    answerField = sprintf(questionAnswerFmt, currentAnswerOrder(indexAnswer));
                    answerChoiceText = sprintf('%s%s', answerChoiceText, passage.(answerField));
                end
                
                for indexQuestionWord = 1:length(questionWords)
                    
                    % For words after the first word in the sentence, write the
                    % blank before writing the word
                    if (indexQuestionWord > 1)
                    
                        if (passage.stimulus{indexQuestionWord}.trigger ...
                                == configuration.pauseTrigger)
                            % the current trigger is special, e.g. start of
                            % sentence; use the pause time from the columns
                            iti = pauseTime;
                        else
                            % take the iti from the word before this
                            iti = passage.stimulus{indexQuestionWord-1}.iti;
                        end
                        
                    elseif (configuration.itiStimulusQuestion >= 0)
                        
                        iti = configuration.itiStimulusQuestion;
                       
                    else
                        
                        iti = passage.stimulus{end}.iti;
                        
                    end
                    
                    [ blockOutput, indexBlockOutput, cumulativeTimestamp ] = ...
                        WriteOutput( ...
                            blockOutput, ...
                            indexBlockOutput, ...
                            cumulativeTimestamp, ...
                            ' ', ... % stimulusText, ...
                            0, ... % trigger, ...
                            iti); 
                
                    word = questionWords{indexQuestionWord}.word;
                    trigger = questionWords{indexQuestionWord}.trigger;
                    duration = questionWords{indexQuestionWord}.duration;

                    [ blockOutput, indexBlockOutput, cumulativeTimestamp ] = ...
                        WriteOutput( ...
                            blockOutput, ...
                            indexBlockOutput, ...
                            cumulativeTimestamp, ...
                            word, ... % stimulusText, ...
                            trigger, ... % trigger, ...
                            duration ); 
                
                end
                
                [ blockOutput, indexBlockOutput, cumulativeTimestamp ] = ...
                    WriteOutput( ...
                        blockOutput, ...
                        indexBlockOutput, ...
                        cumulativeTimestamp, ...
                        '+', ... % stimulusText, ...
                        0, ... % trigger, ...
                        configuration.itiQuestionAnswer); 
                
                % we treat the ordering as digits of a base-m number with m the number of answer
                % options to encode the exact order. This assumes that we
                % have < 4 answers because we need to fit into 256 and some
                % other triggers are taken
                if length(currentAnswerOrder) > 3
                    % we could consider adding additional information by
                    % modifying the trigger on the fixation cross; I'm not
                    % doing that right now because it might mess up
                    % downstream code and I don't think we need this at the
                    % moment
                    error(['The encoding scheme used for recording the order of the answer choices ' ...
                        'will not work for more than 3 answer choices. We need a new encoding scheme :(']);
                end
                
                indexLexigraphic = LexigraphicPermutationIndex(currentAnswerOrder);
                % I've aribtrarily decided that we should error if we
                % cannot reserve the first 10 triggers
                if indexLexigraphic > configuration.questionTriggerBasis - 10
                    error(['Unable to encode the answer order while reserving' ...
                        ' enough triggers given the current questionTriggerBasis'])
                end
                
                questionPromptTrigger = configuration.questionTriggerBasis - indexLexigraphic;
                
                [ blockOutput, indexBlockOutput, cumulativeTimestamp ] = ...
                    WriteOutput( ...
                        blockOutput, ...
                        indexBlockOutput, ...
                        cumulativeTimestamp, ...
                        answerChoiceText, ... % stimulusText, ...
                        questionPromptTrigger, ... % trigger, ...
                        configuration.timePerAsk ); % duration, ...
                    
            end
            
            [ blockOutput, indexBlockOutput, cumulativeTimestamp ] = ...
                WriteOutput( ...
                    blockOutput, ...
                    indexBlockOutput, ...
                    cumulativeTimestamp, ...
                    '+', ... % stimulusText, ...
                    0, ... % trigger, ...
                    finalRestPeriod ); % duration, ...

        end
        
        [ blockOutput, ~, ~ ] = ...
            WriteOutput( ...
                blockOutput, ...
                indexBlockOutput, ...
                cumulativeTimestamp, ...
                '+', ... % stimulusText, ...
                0, ... % trigger, ...
                configuration.timePerFix); % duration, ...                
            
        experiment(indexBlock) = blockOutput; %#ok
            
    end
    
    blockDurations = zeros(length(experiment), 1);
    blockPassageCounts = cell(length(experiment), 1);
    for indexBlock = 1:length(experiment)
        
        blockDuration = 0;
        blockPassageCount = zeros(length(passages), numPauses);
        
        for indexWord = 1:length(experiment(indexBlock).stimulus)
            
            blockDuration = ...
                blockDuration + experiment(indexBlock).duration(indexWord);
            
        end
        
        fprintf('Block %d:\n', indexBlock);
        for indexAssignment = 1:size(blockAssignments{indexBlock}, 1)
            
            indexPassage = blockAssignments{indexBlock}(indexAssignment, 1);
            indexPause = blockAssignments{indexBlock}(indexAssignment, 2);
            
            blockPassageCount(indexPassage, indexPause) = ...
                blockPassageCount(indexPassage, indexPause) + 1;
            
            passage = passages{indexPassage};
            
            pauseField = sprintf(numSecondsPauseFmt, indexPause);
            pauseTime = passage.(pauseField);
            
            pauseInfo = '';
            if (pauseTime > 0)
                pauseInfo = sprintf(', pause %g', pauseTime);
            end
            
            questionInfo = '';
            if (answerOrder{indexBlock}(indexAssignment, 1) ~= 0)
                currentOrder = answerOrder{indexBlock}(indexAssignment, :);
                currentOrder = currentOrder(currentOrder ~= 0);
                questionInfo = ', Q:';
                for indexOrder = 1:length(currentOrder)
                    if (indexOrder > 1)
                        questionInfo = sprintf('%s,', questionInfo);
                    end
                    questionInfo = sprintf('%s%d', questionInfo, currentOrder(indexOrder));
                end
            end
            
            fprintf('Passage %d%s%s\n', ...
                indexPassage, ...
                pauseInfo, ...
                questionInfo);
            
        end
        fprintf('\n');
        
        blockDurations(indexBlock) = blockDuration;
        blockPassageCounts{indexBlock} = blockPassageCount;
        
    end
    
    totalDuration = 0;
    totalPassageCount = zeros(length(passages), numPauses);
    
    for indexBlock = 1:length(experiment)
        
        totalDuration = totalDuration + blockDurations(indexBlock);
        totalPassageCount = totalPassageCount + blockPassageCounts{indexBlock};
        
    end
    
    blockDurations(end+1) = totalDuration;
    blockPassageCounts{end+1} = totalPassageCount;
    
    for indexBlock = 1:(length(experiment) + 1)
        
        if (indexBlock > length(experiment))
            name = 'Totals';
        else
            name = sprintf('Block %d', indexBlock);
        end
        
        fprintf('%s: passage count: %d, duration: %g (%g min):\n', ...
            name, ...
            sum(sum(blockPassageCounts{indexBlock})), ...
            blockDurations(indexBlock), ...
            blockDurations(indexBlock)/60);
        
        for indexPassage = 1:size(blockPassageCounts{indexBlock}, 1)
            
            passage = passages{indexPassage};
            
            for indexPause = 1:size(blockPassageCounts{indexBlock}, 2)
                
                if (blockPassageCounts{indexBlock}(indexPassage, indexPause) == 0)
                    continue;
                end
                
                pauseField = sprintf(numSecondsPauseFmt, indexPause);
                pauseTime = passage.(pauseField);
                
                fprintf('Passage %d, pause %g: %d\n', ...
                    indexPassage, ...
                    pauseTime, ...
                    blockPassageCounts{indexBlock}(indexPassage, indexPause));
            end
            
        end
        fprintf('\n');
    
    end
    
    countPosition1 = zeros(numAnswers, 1);
    numQuestions = 0;
    
    for indexBlock = 1:length(experiment)
        
        indicatorQuestion = answerOrder{indexBlock}(:, 1) ~= 0;
        numQuestions = numQuestions + nnz(indicatorQuestion);
    
        for indexAnswer = 1:numAnswers
            indicator1 = answerOrder{indexBlock}(:, indexAnswer) == 1;
            countPosition1(indexAnswer) = countPosition1(indexAnswer) + nnz(indicator1);
        end
        
    end
    
    for indexAnswer = 1:numAnswers
        fprintf('Questions with answer 1 in position %d: %d\n', indexAnswer, countPosition1(indexAnswer));
    end
    
    fprintf('Total questions: %d\n', numQuestions);
    
    experimentOutputPath = fullfile(subjectDir, 'sentenceBlock.mat');
    save(experimentOutputPath, 'experiment');

end

% make this a function just to guarantee
% that we write all fields out every time
function [ blockOutput, indexBlockOutput, cumulativeTimestamp ] = ...
    WriteOutput( ...
        blockOutput, ...
        indexBlockOutput, ...
        cumulativeTimestamp, ...
        stimulusText, ...
        trigger, ...
        duration )

    blockOutput.stimulus{indexBlockOutput} = stimulusText;
    blockOutput.trigger(indexBlockOutput) = trigger;
    blockOutput.duration(indexBlockOutput) = duration;
    blockOutput.timestamp(indexBlockOutput) = cumulativeTimestamp + duration;
    
    indexBlockOutput = indexBlockOutput + 1;
    cumulativeTimestamp = cumulativeTimestamp + duration;
    
end

function [ sharedDir ] = GetSharedDir()

    fullPath = mfilename('fullpath');
    
    % go up until we get to the shared directory
    [ upOne, namePart, ~ ] = fileparts(fullPath);
    
    while (~isempty(namePart))
        
        if (strcmp(namePart, 'shared'))
            sharedDir = fullPath;
            return;
        end
        
        fullPath = upOne;
        [ upOne, namePart, ~ ] = fileparts(fullPath);
        
    end
    
    error('Unable to find shared directory');

end
