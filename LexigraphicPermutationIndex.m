function [ index ] = LexigraphicPermutationIndex( permutation )
    
    index = 0;
    for j = 1:length(permutation)
        numSmaller = 0;
        for i = (j+1):length(permutation)
            if permutation(i) < permutation(j)
                numSmaller = numSmaller + 1;
            end
        end
        index = index + numSmaller * factorial(length(permutation) - j);
    end
    index = index + 1;
    
end