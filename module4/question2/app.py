import dash
import dash_core_components as dcc
import dash_html_components as html
import plotly.graph_objs as go
import pandas as pd


app = dash.Dash()

df = pd.read_csv("https://raw.githubusercontent.com/AsherMeyers/DATA-608/master/module4/data.csv")
    #'https://gist.githubusercontent.com/chriddyp/cb5392c35661370d95f300086accea51/raw/8e0768211f6b747c0db42a9ce9a0937dafcbd8b2/indicators.csv')

# convert date strings to datetime objects
df['Date'] = pd.to_datetime(df['Date'])


# replace strings of counts with numerics
df['EnteroCount'].replace(to_replace = "<10", value = 9, inplace = True)
df['EnteroCount'].replace(to_replace = "<1", value = 0, inplace = True)
df['EnteroCount'].replace(to_replace = ">24196", value = 24197, inplace = True)
df['EnteroCount'].replace(to_replace = ">2420", value = 2421, inplace = True)
df['EnteroCount'] = pd.to_numeric(df['EnteroCount'])

sites = df['Site'].unique()

app.layout = html.Div([
    html.Div([
        html.Div([
            dcc.Dropdown(
                id='Site1',
                options=[{'label': i, 'value': i} for i in sites],
                value="Gay's Point mid-channel"
            )
        ],
        style={'width': '31%', 'float':'left', 'display': 'inline-block'}),

        html.Div([
            dcc.Dropdown(
                id='Site2',
                options=[{'label': i, 'value': i} for i in sites],
                value="Gowanus Canal"
            )
        ],
        style={'width': '38%', 'float':'center', 'display': 'inline-block'}),

        html.Div([
            dcc.Dropdown(
                id='Site3',
                options=[{'label': i, 'value': i} for i in sites],
                value='Coxsackie Waterfront Park'
            )
        ],style={'width': '31%', 'float': 'right', 'display': 'inline-block'})
    ]),

    dcc.Graph(id='indicator-graphic')
])

@app.callback(
    dash.dependencies.Output('indicator-graphic', 'figure'),
    [dash.dependencies.Input('Site1', 'value'),
     dash.dependencies.Input('Site2', 'value'),
     dash.dependencies.Input('Site3', 'value')])
def update_graph(site1, site2, site3):
    #dff = df[(df['Site'] == site1)|(df['Site'] == site2)|(df['Site'] == site3)]
    sitess = [site1, site2, site3]
    traces = []
    for i in range(3):
        traces.append(go.Scatter(
            x=df[df['Site'] == sitess[i]]["FourDayRainTotal"],
            y=df[df['Site'] == sitess[i]]["EnteroCount"],
            text=sitess[i],
            mode='markers',
            opacity=0.7,
            marker={
                'size': 15,
                'line': {'width': 0.5, 'color': 'white'}
            },
            name=sitess[i]
        ))
    traces.append(go.Scatter(
            x=df[(df['Site'] != sitess[i]) & (df['Site'] != sitess[i]) & (df['Site'] != sitess[i])]["FourDayRainTotal"],
            y=df[(df['Site'] != sitess[i]) & (df['Site'] != sitess[i]) & (df['Site'] != sitess[i])]["EnteroCount"],
            text=df[(df['Site'] != sitess[i]) & (df['Site'] != sitess[i]) & (df['Site'] != sitess[i])]['Site'],
            mode='markers',
            opacity=0.3,
            marker={
                'size': 2.5,
                'color': 'grey',
                'line': {'width': 0.5, 'color': 'grey'}
            },
            name='Others'
        ))
    return {
        'data': traces,
        'layout': go.Layout(
            xaxis={
                'title': 'Four Day Rain Total',
                'type': 'linear'
            },
            yaxis={
                'title': 'Entero Count',
                'type': 'log' 
            },
            margin={'l': 50, 'b': 40, 't': 10, 'r': 0},
            hovermode='closest'
        )
    }


if __name__ == '__main__':
    app.run_server()
