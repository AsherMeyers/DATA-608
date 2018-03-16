"""
Created on Mon Mar 12 19:44:48 2018

@author: asher

Youre a civic hacker and kayak enthusiast who just came across this dataset.
Youd like to create an app that recommends launch sites to users. Ideally an app
like this will use live data to give current recommendations, but youre still in
the testing phase. Create a prototype that allows a user to pick a date, and will 
give its recommendations for that particular date.

Think about your recommendations . Youre given federal guidelines above, but
you may still need to make some assumptions about which sites to recommend. 
Consider your audience. Users will appreciate some information explaining why a 
particular site is flagged as unsafe, but theyre not scientists.
"""
import pandas as pd
import dash
import dash_core_components as dcc
import dash_html_components as html
import datetime
import plotly.graph_objs as go
from colour import Color

# Grab and scrub the data 
df = pd.read_csv("https://raw.githubusercontent.com/AsherMeyers/DATA-608/master/module4/data.csv")

# convert date strings to datetime objects
df['Date'] = pd.to_datetime(df['Date'])


# replace strings of counts with numerics
df['EnteroCount'].replace(to_replace = "<10", value = 9, inplace = True)
df['EnteroCount'].replace(to_replace = "<1", value = 0, inplace = True)
df['EnteroCount'].replace(to_replace = ">24196", value = 24197, inplace = True)
df['EnteroCount'].replace(to_replace = ">2420", value = 2421, inplace = True)
df['EnteroCount'] = pd.to_numeric(df['EnteroCount'])


# Since min(df.SampleCount) > 5, i.e. all samples are at least 5,
# the threshold for Entero Counts is 30 - if a place's count exceeds 30 on a 
# given date, it is unsafe.

# Start and End of observations
start = min(df.Date)
end = max(df.Date)

userdate = datetime.datetime(2013, 10, 1, 0, 0, 0)
place = "Hudson above Mohawk River"

def EnteroAvgCount(place, userdate):
    # Identify closest two dates
    # last observation before user's selected date
    try:
        date1 = max(df.Date[(df.Site == place) & (df.Date <= userdate)]) 
    except (ValueError):
        date2 = min(df.Date[(df.Site == place) & (df.Date >= userdate)])
        return df[(df.Site == place) & ((df.Date == date2))].EnteroCount.iloc[0]
    
    # first observation after user selected date
    try: 
        date2 = min(df.Date[(df.Site == place) & (df.Date >= userdate)])
    except (ValueError):
        return df[(df.Site == place) & ((df.Date == date1))].EnteroCount.iloc[0]
    
    
    dff = df[(df.Site == place) & ((df.Date == date2) | (df.Date == date1))]
    dff = dff.sort_values(by='Date')
    
    date1diff = abs((userdate - date1).days)
    date2diff = abs((userdate - date2).days)
    
    # In case user picks exactly the date of an observation
    if date1diff == 0:
        return dff.EnteroCount.iloc[0]
    elif date2diff == 0:
        return dff.EnteroCount.iloc[1]
    elif date2 == None:
        return dff.EnteroCount.iloc[0]
    
    # Otherwise:
    else:
        # Weights for  observations, based on proximity to user selected date
        date1wt = date2diff/(date1diff + date2diff)
        date2wt = date1diff/(date1diff + date2diff)
        
        # Weights by number of samples for each observation
        numSamples = dff.SampleCount.iloc[0] + dff.SampleCount.iloc[1]
        sample1wt = dff.SampleCount.iloc[0]/numSamples
        sample2wt = dff.SampleCount.iloc[1]/numSamples
        
        # Calculate weights by observation
        obs1wt = date1wt * sample1wt
        obs2wt = date2wt * sample2wt
        
        # Recalculate weights to add up to 1.
        multiplier = 1/(obs1wt + obs2wt)
        obs1wt *= multiplier
        obs2wt *= multiplier
        
        # Estimated Count
        est = dff.EnteroCount.iloc[0]*obs1wt + dff.EnteroCount.iloc[1]*obs2wt
        
        return est
    
def CleanSites(userdate, n = 30):
    sites = df.Site.unique()
    estimates = len(sites)*[None]
    for i in range(len(sites)):
        estimates[i] = EnteroAvgCount(sites[i], userdate)
    
    data = {'EnteroCount': estimates,
            'Site': sites
            }
    estimates = pd.DataFrame(data)
    cleans = estimates[estimates['EnteroCount'] < 30]
    cleans = cleans[['Site', 'EnteroCount']]
    cleans.iloc[:,1] = round(cleans.iloc[:,1],2)
    cleans.sort_values(by=['EnteroCount'], ascending=False, inplace = True)
    
    return cleans.tail(n)
        
    
    
def generate_table(userdate): #place
    
    # Identify closest two dates
    #date1 = max(df.Date[(df.Site == place) & (df.Date <= userdate)]) # last observation before user's selected date
    #date2 = min(df.Date[(df.Site == place) & (df.Date >= userdate)]) # first observation after user selected date
    
    #dataframe = df[(df.Site == place) & ((df.Date == date2) | (df.Date == date1))]
    #dataframe = dataframe.sort_values(by='Date')
    dataframe = CleanSites(userdate)
    
    return html.Table(
        # Header
        [html.Tr([html.Th(col) for col in dataframe.columns])] +

        # Body
        [html.Tr([
            html.Td(dataframe.iloc[i][col]) for col in dataframe.columns
        ]) for i in range(min(len(dataframe), 75))]
    )

df1 = CleanSites(userdate)


app = dash.Dash()
orange = Color("orange")
colorlist = list(orange.range_to(Color("blue"),len(df1)))
colors = len(df1)*[None]
for i in range(len(df1)): colors[i] = colorlist[i].hex_l

app.layout = html.Div(children=[
            dcc.DatePickerSingle(
        id='my-date-picker-single',
        min_date_allowed=start,
        max_date_allowed=end,
        initial_visible_month=dt(2010, 1, 1),
        date=dt(2010, 1, 1),
        
        dcc.Graph(
        id='Clean Sites',
        figure={
            'data': [
                    
                go.Bar(
                    y=list(df1.Site),
                    x=list(df1.EnteroCount),
                    
                    #mode='markers',
                    opacity=0.6,
                    orientation='h',
                    marker=dict(color=colors)#,
                                #line = dict(
                                #        color = colors,
                                #        width = 1))
                    #marker={
                    #    'size': 15,
                    
                    #    'line': {'width': 0.5, 'color': 'white'}
                    #},
                    #name=i
                ) #for i in df1.continent.unique()
            ],
            'layout': go.Layout(
                yaxis={'title': 'Site'}, #'type': 'log',
                xaxis={'title': 'Estimated Entero Count'},
                margin={'l': 250, 'b': 40, 't': 10, 'r': 10},
                legend={'x': 0, 'y': 1},
                hovermode='closest',
                font={'size':8}#,
                #titlefont={'size':24}
            )
        }
    ),
    html.H4(children='Clean Sites'),
    generate_table(userdate, 75)
])
    



if __name__ == '__main__':
    app.run_server(debug=True)
    

