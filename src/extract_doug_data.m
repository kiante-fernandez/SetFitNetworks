%Preparing data for analysis in R
%Lee, D. G., & Holyoak, K. J. Coherence shifts in attribute evaluations.
%Decision, 8(4), 257. https://doi.org/10.1037/dec0000151

%how many subjects does the dataset have?
total_subjects = [experiment1.nSubs,experiment2.nSubs,experiment3.nSubs,experiment4.nSubs,experiment5.nSubs]
sum(total_subjects) %325 subjects

nSubs = total_subjects(1)

ratings = zeros(nSubs,size(experiment1.itemSet,1));


for sub_idx = 1:nSubs
    ratings(sub_idx,:) = experiment1.item{1,sub_idx}.rating1;
end

total_subjects = [experiment1.nSubs,experiment2.nSubs,experiment3.nSubs,experiment4.nSubs,experiment5.nSubs];

rating1_data = zeros(0,size(experiment2.itemSet,1));
rating2_data = zeros(0,size(experiment2.itemSet,1));

nutrition1_data =  zeros(0,size(experiment2.itemSet,1));
%nutrition2_data =  zeros(0,size(experiment2.itemSet,1));

pleasure1_data =  zeros(0,size(experiment2.itemSet,1));
%pleasure2_data =  zeros(0,size(experiment2.itemSet,1));


for exp_idk = 2:5
    nSubs = total_subjects(exp_idk);
    switch exp_idk
        
        case 2
            temp =  zeros(nSubs ,size(experiment2.itemSet,1));
            temp2 = zeros(nSubs ,size(experiment2.itemSet,1));
            temp3 = zeros(nSubs ,size(experiment2.itemSet,1));
            temp4 = zeros(nSubs ,size(experiment2.itemSet,1));
            
            for sub_idx = 1:nSubs
                temp(sub_idx,:) =  experiment2.item{1,sub_idx}.rating1;
                temp2(sub_idx,:) = experiment2.item{1,sub_idx}.rating2;
                temp3(sub_idx,:) = experiment2.item{1,sub_idx}.nutrition1;
                temp4(sub_idx,:) = experiment2.item{1,sub_idx}.pleasure1;
            end
            
        case 3
            temp =  zeros(nSubs ,size(experiment3.itemSet,1));
            temp2 = zeros(nSubs ,size(experiment3.itemSet,1));
            temp3 = zeros(nSubs ,size(experiment3.itemSet,1));
            temp4 = zeros(nSubs ,size(experiment3.itemSet,1));
            
            for sub_idx = 1:nSubs
                temp(sub_idx,:) =  experiment3.item{1,sub_idx}.rating1;
                temp2(sub_idx,:) = experiment3.item{1,sub_idx}.rating2;
                temp3(sub_idx,:) = experiment3.item{1,sub_idx}.nutrition1;
                temp4(sub_idx,:) = experiment3.item{1,sub_idx}.pleasure1;
            end
            
        case 4
            temp =  zeros(nSubs ,size(experiment4.itemSet,1));
            temp2 = zeros(nSubs ,size(experiment4.itemSet,1));
            temp3 = zeros(nSubs ,size(experiment4.itemSet,1));
            temp4 = zeros(nSubs ,size(experiment4.itemSet,1));
            
            for sub_idx = 1:nSubs
                temp(sub_idx,:) =  experiment4.item{1,sub_idx}.rating1;
                temp2(sub_idx,:) = experiment4.item{1,sub_idx}.rating2;
                temp3(sub_idx,:) = experiment4.item{1,sub_idx}.nutrition1;
                temp4(sub_idx,:) = experiment4.item{1,sub_idx}.pleasure1;
            end
            
        case 5
            temp =  zeros(nSubs ,size(experiment5.itemSet,1));
            temp2 = zeros(nSubs ,size(experiment5.itemSet,1));
            temp3 = zeros(nSubs ,size(experiment5.itemSet,1));
            temp4 = zeros(nSubs ,size(experiment5.itemSet,1));
            
            for sub_idx = 1:nSubs
                temp(sub_idx,:) =  experiment5.item{1,sub_idx}.rating1;
                temp2(sub_idx,:) = experiment5.item{1,sub_idx}.rating2;
                temp3(sub_idx,:) = experiment5.item{1,sub_idx}.nutrition1;
                temp4(sub_idx,:) = experiment5.item{1,sub_idx}.pleasure1;
            end
    end
    rating1_data =  [rating1_data; temp] ;
    rating2_data =  [rating2_data; temp2] ;
    nutrition1_data = [nutrition1_data; temp3] ;
    pleasure1_data = [pleasure1_data; temp4] ;
    
end

csvwrite('/Users/kiantefernandez/Documents/OSU/Karmarkar_2021_subset_choice/multi_select/data/lee_2021_rating1.csv',rating1_data);
csvwrite('/Users/kiantefernandez/Documents/OSU/Karmarkar_2021_subset_choice/multi_select/data/lee_2021_rating2.csv',rating2_data);
csvwrite('/Users/kiantefernandez/Documents/OSU/Karmarkar_2021_subset_choice/multi_select/data/lee_2021_nutrition1.csv',nutrition1_data);
csvwrite('/Users/kiantefernandez/Documents/OSU/Karmarkar_2021_subset_choice/multi_select/data/lee_2021_pleasure1.csv',pleasure1_data);


%% extract choice proportions
total_subjects = [experiment1.nSubs,experiment2.nSubs,experiment3.nSubs,experiment4.nSubs,experiment5.nSubs];

experiment2.nChoices %30 choices each
experiment3.nChoices
experiment4.nChoices
experiment5.nChoices

choice_data = zeros(0,experiment2.nChoices);

%% code Left option as best option
nSubs = 63
total_subjects = [experiment1.nSubs,experiment2.nSubs,experiment3.nSubs,experiment4.nSubs,experiment5.nSubs]

chosen = zeros(nSubs,experiment2.nChoices);

choice_data =  zeros(0,experiment2.nChoices);

for exp_idk = 2:5
    nSubs = total_subjects(exp_idk);
    chosen = zeros(nSubs,experiment2.nChoices);
    
    switch exp_idk
        
        case 2 %for exp 2 data
            for sub_idx = 1:nSubs
                %get subject data
                temp = [experiment2.choice{1,sub_idx}.itemL' experiment2.choice{1,sub_idx}.itemR' experiment2.choice{1,sub_idx}.choice'];
                %for each choice trial
                for foo = 1:30
                    %if the choice is 1 get the right item ID, OW get left
                    if temp(foo,3) == 1
                        chosen(sub_idx,foo) = temp(foo, 2);
                    else
                        chosen(sub_idx,foo) = temp(foo, 1);
                    end
                    
                end
            end
        case 3 %for exp 3 data
            for sub_idx = 1:nSubs
                %get subject data
                temp = [experiment3.choice{1,sub_idx}.itemL' experiment3.choice{1,sub_idx}.itemR' experiment3.choice{1,sub_idx}.choice'];
                %for each choice trial
                for foo = 1:30
                    %if the choice is 1 get the right item ID, OW get left
                    if temp(foo,3) == 1
                        chosen(sub_idx,foo) = temp(foo, 2);
                    else
                        chosen(sub_idx,foo) = temp(foo, 1);
                    end
                    
                end
            end
        case 4 %for exp 4 data
            for sub_idx = 1:nSubs
                %get subject data
                temp = [experiment4.choice{1,sub_idx}.itemL' experiment4.choice{1,sub_idx}.itemR' experiment4.choice{1,sub_idx}.choice'];
                %for each choice trial
                for foo = 1:30
                    %if the choice is 1 get the right item ID, OW get left
                    if temp(foo,3) == 1
                        chosen(sub_idx,foo) = temp(foo, 2);
                    else
                        chosen(sub_idx,foo) = temp(foo, 1);
                    end
                    
                end
            end
        case 5 %for exp 5 data
            for sub_idx = 1:nSubs
                %get subject data
                temp = [experiment5.choice{1,sub_idx}.itemL' experiment5.choice{1,sub_idx}.itemR' experiment5.choice{1,sub_idx}.choice'];
                %for each choice trial
                for foo = 1:30
                    %if the choice is 1 get the right item ID, OW get left
                    if temp(foo,3) == 1
                        chosen(sub_idx,foo) = temp(foo, 2);
                    else
                        chosen(sub_idx,foo) = temp(foo, 1);
                    end
                    
                end
            end
    end
    choice_data =   [choice_data ; chosen];
end

csvwrite('/Users/kiantefernandez/Documents/OSU/Karmarkar_2021_subset_choice/multi_select/data/lee_2021_choice_exp2_5.csv',choice_data);



