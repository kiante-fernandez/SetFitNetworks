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
rating3_data = zeros(0,size(experiment2.itemSet,1));

nutrition1_data =  zeros(0,size(experiment2.itemSet,1));
%nutrition2_data =  zeros(0,size(experiment2.itemSet,1));

pleasure1_data =  zeros(0,size(experiment2.itemSet,1));
%pleasure2_data =  zeros(0,size(experiment2.itemSet,1));

rating1_RT_data = zeros(0,size(experiment2.itemSet,1));
rating2_RT_data = zeros(0,size(experiment2.itemSet,1));
rating3_RT_data = zeros(0,size(experiment2.itemSet,1));

for exp_idk = 2:5
    nSubs = total_subjects(exp_idk);
    switch exp_idk
        
        case 2
            temp =  zeros(nSubs ,size(experiment2.itemSet,1));
            temp2 = zeros(nSubs ,size(experiment2.itemSet,1));
            temp3 = zeros(nSubs ,size(experiment2.itemSet,1));
            temp4 = zeros(nSubs ,size(experiment2.itemSet,1));
            temp5 = zeros(nSubs ,size(experiment2.itemSet,1));
            
            temp6 = zeros(nSubs ,size(experiment2.itemSet,1));
            temp7 = zeros(nSubs ,size(experiment2.itemSet,1));
            temp8 = zeros(nSubs ,size(experiment2.itemSet,1));
            
            for sub_idx = 1:nSubs
                temp(sub_idx,:) =  experiment2.item{1,sub_idx}.rating1;
                temp2(sub_idx,:) = experiment2.item{1,sub_idx}.rating2;
                temp3(sub_idx,:) = experiment2.item{1,sub_idx}.nutrition1;
                temp4(sub_idx,:) = experiment2.item{1,sub_idx}.pleasure1;
                temp5(sub_idx,:) = experiment2.item{1,sub_idx}.rating3;
                
                temp6(sub_idx,:) = experiment2.item{1,sub_idx}.RT_rating1;
                temp7(sub_idx,:) = experiment2.item{1,sub_idx}.RT_rating2;
                temp8(sub_idx,:) = experiment2.item{1,sub_idx}.RT_rating3;

            end
            
        case 3
            temp =  zeros(nSubs ,size(experiment3.itemSet,1));
            temp2 = zeros(nSubs ,size(experiment3.itemSet,1));
            temp3 = zeros(nSubs ,size(experiment3.itemSet,1));
            temp4 = zeros(nSubs ,size(experiment3.itemSet,1));
            temp5 = zeros(nSubs ,size(experiment2.itemSet,1));
            
            temp6 = zeros(nSubs ,size(experiment3.itemSet,1));
            temp7 = zeros(nSubs ,size(experiment3.itemSet,1));
            temp8 = zeros(nSubs ,size(experiment3.itemSet,1));

            for sub_idx = 1:nSubs
                temp(sub_idx,:) =  experiment3.item{1,sub_idx}.rating1;
                temp2(sub_idx,:) = experiment3.item{1,sub_idx}.rating2;
                temp3(sub_idx,:) = experiment3.item{1,sub_idx}.nutrition1;
                temp4(sub_idx,:) = experiment3.item{1,sub_idx}.pleasure1;
                temp5(sub_idx,:) = experiment3.item{1,sub_idx}.rating3;
                
                temp6(sub_idx,:) = experiment3.item{1,sub_idx}.RT_rating1;
                temp7(sub_idx,:) = experiment3.item{1,sub_idx}.RT_rating2;
                temp8(sub_idx,:) = experiment3.item{1,sub_idx}.RT_rating3;
            end
            
        case 4
            temp =  zeros(nSubs ,size(experiment4.itemSet,1));
            temp2 = zeros(nSubs ,size(experiment4.itemSet,1));
            temp3 = zeros(nSubs ,size(experiment4.itemSet,1));
            temp4 = zeros(nSubs ,size(experiment4.itemSet,1));
            temp5 = zeros(nSubs ,size(experiment4.itemSet,1));
            
            temp6 = zeros(nSubs ,size(experiment4.itemSet,1));
            temp7 = zeros(nSubs ,size(experiment4.itemSet,1));
            temp8 = zeros(nSubs ,size(experiment4.itemSet,1));

            for sub_idx = 1:nSubs
                temp(sub_idx,:) =  experiment4.item{1,sub_idx}.rating1;
                temp2(sub_idx,:) = experiment4.item{1,sub_idx}.rating2;
                temp3(sub_idx,:) = experiment4.item{1,sub_idx}.nutrition1;
                temp4(sub_idx,:) = experiment4.item{1,sub_idx}.pleasure1;
                temp5(sub_idx,:) = experiment4.item{1,sub_idx}.rating3;
                
                temp6(sub_idx,:) = experiment4.item{1,sub_idx}.RT_rating1;
                temp7(sub_idx,:) = experiment4.item{1,sub_idx}.RT_rating2;
                temp8(sub_idx,:) = experiment4.item{1,sub_idx}.RT_rating3;
            end
            
        case 5
            temp =  zeros(nSubs ,size(experiment5.itemSet,1));
            temp2 = zeros(nSubs ,size(experiment5.itemSet,1));
            temp3 = zeros(nSubs ,size(experiment5.itemSet,1));
            temp4 = zeros(nSubs ,size(experiment5.itemSet,1));
            temp5 = zeros(nSubs ,size(experiment5.itemSet,1));
            
            temp6 = zeros(nSubs ,size(experiment5.itemSet,1));
            temp7 = zeros(nSubs ,size(experiment5.itemSet,1));
            temp8 = zeros(nSubs ,size(experiment5.itemSet,1));

            for sub_idx = 1:nSubs
                temp(sub_idx,:) =  experiment5.item{1,sub_idx}.rating1;
                temp2(sub_idx,:) = experiment5.item{1,sub_idx}.rating2;
                temp3(sub_idx,:) = experiment5.item{1,sub_idx}.nutrition1;
                temp4(sub_idx,:) = experiment5.item{1,sub_idx}.pleasure1;
                temp5(sub_idx,:) = experiment5.item{1,sub_idx}.rating3;
                
                temp6(sub_idx,:) = experiment5.item{1,sub_idx}.RT_rating1;
                temp7(sub_idx,:) = experiment5.item{1,sub_idx}.RT_rating2;
                temp8(sub_idx,:) = experiment5.item{1,sub_idx}.RT_rating3;
            end
    end
    rating1_data =  [rating1_data; temp] ;
    rating2_data =  [rating2_data; temp2] ;
    rating3_data =  [rating3_data; temp5] ;

    nutrition1_data = [nutrition1_data; temp3] ;
    pleasure1_data = [pleasure1_data; temp4] ;
    
    rating1_RT_data =  [rating1_RT_data; temp6] ;
    rating2_RT_data =  [rating2_RT_data; temp7] ;
    rating3_RT_data =  [rating3_RT_data; temp8] ;
    
end

csvwrite('/Users/kiantefernandez/Documents/OSU/SetFitNetworks/data/lee_2021_rating1.csv',rating1_data);
csvwrite('/Users/kiantefernandez/Documents/OSU/SetFitNetworks/data/lee_2021_rating2.csv',rating2_data);
csvwrite('/Users/kiantefernandez/Documents/OSU/SetFitNetworks/data/lee_2021_rating3.csv',rating3_data);

csvwrite('/Users/kiantefernandez/Documents/OSU/SetFitNetworks/data/lee_2021_nutrition1.csv',nutrition1_data);
csvwrite('/Users/kiantefernandez/Documents/OSU/SetFitNetworks/data/lee_2021_pleasure1.csv',pleasure1_data);

csvwrite('/Users/kiantefernandez/Documents/OSU/SetFitNetworks/data/lee_2021_rating_RT1.csv',rating1_RT_data);
csvwrite('/Users/kiantefernandez/Documents/OSU/SetFitNetworks/data/lee_2021_rating_RT2.csv',rating2_RT_data);
csvwrite('/Users/kiantefernandez/Documents/OSU/SetFitNetworks/data/lee_2021_rating_RT3.csv',rating3_RT_data);

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

%%
%% extract choice proportions
clear all

load('Lee_Holyoak_2021.mat')

total_subjects = [experiment1.nSubs,experiment2.nSubs,experiment3.nSubs,experiment4.nSubs,experiment5.nSubs];

experiments = {experiment2,experiment3,experiment4,experiment5};

choice_data =  zeros(0,experiment2.nChoices);


% exp_idk = 1
for exp_idk = 1:4
    
    unpackStruct(experiments{exp_idk}) %get the data from a single expetiment
    
    exp_temp = repmat(exp_idk + 1, 30, 1);
    
    % code Left option as best option
    for s=1:nSubs
        
        for i=1:length(choice{s}.choice)
            if choice{s}.ratingL1(i)>choice{s}.ratingR1(i)
                tmp=[choice{s}.ratingL1(i) choice{s}.ratingR1(i) choice{s}.pleasureL1(i) choice{s}.pleasureR1(i) choice{s}.nutritionL1(i) choice{s}.nutritionR1(i) choice{s}.ratingL2(i) choice{s}.ratingR2(i) choice{s}.pleasureL2(i) choice{s}.pleasureR2(i) choice{s}.nutritionL2(i) choice{s}.nutritionR2(i)];
                choice{s}.ratingL1(i)=tmp(2);
                choice{s}.ratingR1(i)=tmp(1);
                choice{s}.pleasureL1(i)=tmp(4);
                choice{s}.pleasureR1(i)=tmp(3);
                choice{s}.nutritionL1(i)=tmp(6);
                choice{s}.nutritionR1(i)=tmp(5);
                choice{s}.ratingL2(i)=tmp(8);
                choice{s}.ratingR2(i)=tmp(7);
                choice{s}.pleasureL2(i)=tmp(10);
                choice{s}.pleasureR2(i)=tmp(9);
                choice{s}.nutritionL2(i)=tmp(12);
                choice{s}.nutritionR2(i)=tmp(11);
                choice{s}.choice(i)=abs(choice{s}.choice(i)-1);
                choice{s}.rDiff1(i)=-choice{s}.rDiff1(i);
                choice{s}.pDiff1(i)=-choice{s}.pDiff1(i);
                choice{s}.nDiff1(i)=-choice{s}.nDiff1(i);
                choice{s}.rDiff2(i)=-choice{s}.rDiff2(i);
                choice{s}.pDiff2(i)=-choice{s}.pDiff2(i);
                choice{s}.nDiff2(i)=-choice{s}.nDiff2(i);
            end
        end
        subject_temp = repmat(s + exp_idk * 100, 30, 1);
        temp = [subject_temp exp_temp choice{1,s}.itemL' choice{1,s}.itemR' choice{1,s}.ratingL1' choice{1,s}.ratingR1' choice{1,s}.rDiff1' choice{1,s}.choice' choice{1,s}.RT'];
        choice_data =   [choice_data ; temp];
    end
    
    %clear for the next data set
    clear choice item itemSet nChoices nItems nSubs subID
end

csvwrite('/Users/kiantefernandez/Documents/OSU/SetFitNetworks/data/lee_2021_exp2_5.csv',choice_data);
