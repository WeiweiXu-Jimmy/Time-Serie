#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd


# ## Séries non désaisonnalisées (fr=France, be=Belgique, nl= paus-bas, ger=Allemagne, uk=Grande-Bretagne)

# In[2]:


df_day = pd.read_csv('/Users/xudawei/Desktop/devoirs en M2/Fwd ETUDE DE CAS/Serie temporelle/Series price élec/Day ahead-表格 1.csv',sep =",")
df_day


# In[3]:


df_month = pd.read_csv('/Users/xudawei/Desktop/devoirs en M2/Fwd ETUDE DE CAS/Serie temporelle/Series price élec/Month ahead-表格 1.csv',sep =",")
df_month


# In[4]:


df_year = pd.read_csv('/Users/xudawei/Desktop/devoirs en M2/Fwd ETUDE DE CAS/Serie temporelle/Series price élec/Year ahead-表格 1.csv',sep =",")
df_year


# # Analyse sur le day_data (France-Germany)

# In[5]:


df_day.head()


# # Question 1: expliquez ce que sont des prix (ou des contrats) de type AHEAD (day-ahead, etc.)
#     
#    ils sont les prix prévisionnels dans les journées prochaines       

# # Question 2 : faites une analyse descriptive simple de ces données et tracez leur évolution dans le temps.
# 

# In[6]:


df_d = df_day[['date','fr_da_base','ger_da_base']]
df_m = df_month[['date','fr_ma_base','ger_ma_base']]
df_y = df_year[['date','fr_ya_base','ger_ya_base']]
df_d.head()


# In[7]:


df_d.isnull().sum()


# In[8]:


df_m.isnull().sum()


# In[9]:


df_y.isnull().sum()


# In[10]:


df_d.dtypes


# In[11]:


df_m.dtypes


# In[12]:


df_y.dtypes


# In[13]:


df_d.shape


# In[14]:


df_m.shape


# In[15]:


df_y.shape


# In[16]:


index = pd.to_datetime(df_d.date)
df_d.index = index
df_d.index


# In[17]:


index = pd.to_datetime(df_m.date)
df_m.index = index
df_m.index


# In[18]:


index = pd.to_datetime(df_y.date)
df_y.index = index
df_y.index


# In[ ]:





# In[19]:


df_d.head()


# In[20]:


df_m.head()


# In[21]:


df_y.head()


# In[22]:


df_d=df_d.rename(columns = {'fr_da_base':'France', 'ger_da_base':'Germany'})
df_d.head()


# In[23]:


df_m=df_m.rename(columns = {'fr_ma_base':'France', 'ger_ma_base':'Germany'})
df_m.head()


# In[ ]:


df_y=df_y.rename(columns = {'fr_ya_base':'France', 'ger_ya_base':'Germany'})
df_y.head()


# In[ ]:


df_d.describe()


# In[ ]:


# Reduce the precision of numbers - so that it is easy to read
#pd.set_option('precision', 0)


# In[ ]:


df_d.describe()


# In[ ]:


df_m.describe()


# In[ ]:


df_y.describe()


# In[ ]:


df_d['France'].plot()


# In[ ]:


df_d['Germany'].plot()


# In[ ]:


df_d.plot(kind ='line', y = ['France','Germany'])


# In[ ]:


import dash
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd
import plotly.graph_objs as go
import plotly.express as px


# In[ ]:


external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)


# In[ ]:


country_list = ['France','Germany']
date_list = ['Day','Month','Year']


# In[ ]:


app.layout = html.Div([
    
    html.Div([
        html.Br(),
        html.Label([''],style={'font-weight': 'bold', "text-align": "center"}),
        
        dcc.Dropdown(
            id = 'country',
            options = [{'label': i, 'value':i} for i in country_list],
            value=country_list[0]),
        
        
        dcc.RadioItems(
        id = 'date',
        options = [{'label': i, 'value': i} for i in date_list],
        value = date_list[0],
        labelStyle = {'display': 'inline-block'})
        

    ],style = {'width':'25%','margin-left':'0%'}),
    
    html.Div([
        dcc.Graph(id = 'our_serie')
        
    ], className = 'nine columns'),
    
    
],style ={'display': 'inline-block','width':"99%"})


# In[ ]:


df_y.head()


# In[ ]:


df_m.head()


# In[ ]:


df_d.head()


# In[ ]:


@app.callback(
    dash.dependencies.Output('our_serie','figure'),
    [dash.dependencies.Input('country','value'),
    dash.dependencies.Input('date','value')])

def build_graph(country,date):
    
    if date == date_list[0]:
        dff  = df_d
    elif date == date_list[1]:
        dff  = df_m
    else:
        dff = df_y
        
       
    if country == country_list[0]:
        fig = px.line(x=dff['date'], y = dff['France'],height=600)
    else:
        fig = px.line(x=dff['date'], y = dff['Germany'],height=600)
                  
    fig.update_layout(yaxis={'title':'Degree'},xaxis={'title':'Date'},
                      title={'text':"Séries temporelle de la consommation d'électricité en Allemagne et en France",
                      'font':{'size':18},'x':0.5,'xanchor':'center'})
    return fig


# In[ ]:


if __name__ == '__main__':
    app.run_server(debug=False,host = '0.0.0.0', port = 8010)


# In[ ]:



        


# In[ ]:





# In[ ]:




