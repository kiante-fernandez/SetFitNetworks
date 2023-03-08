import os
import openai
import pandas as pd
import re

df = pd.read_excel(r'/Users/kiantefernandez/Documents/OSU/SetFitNetworks/data/snackitemnames_nicholas/item_image_numbers_exp2_5_nicholas.xlsx')

openai.api_key = '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'

food_items = df.Name
food_items = food_items[0:2]
ans = []

ans = pd.DataFrame(columns=food_items, index= food_items)

for x in food_items:
    question =  "On a scale from -100 to 100 how similar is " + x + " to each of the follow foods:\n\norange\nplum\nblackberry\npeach\ncherry\nraspberry\napple\ngrape\ngolden delicious apple\nhoneydew melon\nkiwi\ngreen bell pepper\ncarrot\npeanuts\nalmonds\nbaguette\nsliced  loaf bread\nboule bread\nrye bread\ndeli turkey\nchicken tenders\nmeatballs\ngouda\nswiss cheese\npotato chips\ntortilla chips\ndoritos\nritz cracker\nsaltine cracker\nwheat thins\negg rolls \nfrench fries\nbuttered popcorn\ntriscuits\nwavy lays\nchocolate ice cream cone\nstrawberry ice cream cone\nvanilla soft serve cone\nnutrigrain bar\nchurro\ntwist donut\ntuile cookie\nmadeleine\noatmeal raisin cookie\nthumprint cookie\nsugar cookie\nvanilla wafer\nchocolate wafer\nchocolate bark\ndark chocolate\nmilk chocolate\npocky\ntoblerone\nlindt lindor chocolate truffle\ntwix\ncaramel\npeanut m&m's\nbrownie\nfrosted brownie\nlemon cake\n\nsimilarity ratings:\n",
    response_temp = openai.Completion.create(
        model="text-davinci-003",
        prompt= question,
        temperature=0,
        max_tokens=522,
        top_p=1,
        frequency_penalty=0,
        presence_penalty=0
        )
    # Define the pattern for matching numbers
    pattern = r'-?\d+'
    # Extract all the numbers from the string
    numbers = re.findall(pattern, response_temp['choices'][0]['text'])
    # Convert the numbers from strings to integers
    numbers = [int(n) for n in numbers]
    ans.loc[x] = numbers 
    
#save output
ans.to_csv('data/Lee_Holyoak_GPT3.csv', index=False) 

