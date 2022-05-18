from ipywidgets import interact, DatePicker
import cufflinks as cf
import pandas as pd
from auroraPSI.observation_plotter import ObservationPlotter
from IPython import get_ipython
from matplotlib import pyplot as plt


class PlotterAdapter(ObservationPlotter):

    def date_plot(self):
        interact(self._date_plot)

    def histogram_plot(self):
        interact(self._histogram_plot)

    def scatter_plot(self):
        interact(self._scatter_plot)

    def all_plot(self):
        ipython = get_ipython()
        ipython.magic("matplotlib widget")
        plt.style.use("seaborn-whitegrid")
        self._df.plot()
        plt.legend(bbox_to_anchor=(1.04, 1.2), loc="upper left")

    def __init__(self, observation_df: pd.DataFrame):
        self._df = observation_df.copy()
        cf.go_offline()

        self._scatter_plot = self._init_scatter_plot()
        self._date_plot = self._init_date_plot()
        self._histogram_plot = self._init_histogram_plot()

    def _init_scatter_plot(self):
        def _scatter_plot(x=list(self._df.columns),
                          y=list(self._df.select_dtypes('number').columns)[1:],
                          theme=list(cf.themes.THEMES.keys()),
                          colorscale=list(cf.colors._scales_names.keys())):
            self._df.iplot(kind='scatter', x=x, y=y, mode='markers',
                           xTitle=x.title(), yTitle=y.title(),
                           # text='title',
                           title=f'{y.title()} vs {x.title()}',
                           theme=theme, colorscale=colorscale)

        return _scatter_plot

    def _init_date_plot(self):
        def _date_plot(y=list(self._df.select_dtypes('number').columns)[1:],
                       theme=list(cf.themes.THEMES.keys()),
                       colorscale=list(cf.colors._scales_names.keys()),
                       start_date=DatePicker(
                           description='Start Date',
                           disabled=False,
                           value=pd.to_datetime(self._df.index[0])
                       ),
                       end_date=DatePicker(
                           description='End Date',
                           disabled=False,
                           value=pd.to_datetime(self._df.index[-1])
                       )
                       ):
            self._df.loc[pd.Timestamp(start_date):pd.Timestamp(end_date)] \
                .iplot(kind='scatter', y=y, mode='markers', xTitle="date", yTitle=y.title(),
                       title=f'{y.title()}', theme=theme, colorscale=colorscale)

        return _date_plot

    def _init_histogram_plot(self):
        def _histogram_plot(x=list(self._df.select_dtypes('number').columns),
                            theme=list(cf.themes.THEMES.keys()),
                            colorscale=list(cf.colors._scales_names.keys())):
            self._df[str(x)].iplot(kind='histogram',
                                   xTitle=x.title(),
                                   title=f'histogram {x.title()}',
                                   theme=theme, colorscale=colorscale)

        return _histogram_plot

    @property
    def data_frame(self):
        return self._df

    @data_frame.setter
    def data_frame(self, observation_df: pd.DataFrame):
        self._df = observation_df.copy()
        cf.go_offline()
        self._scatter_plot = self._init_scatter_plot()
        self._date_plot = self._init_date_plot()
        self._histogram_plot = self._init_histogram_plot()
