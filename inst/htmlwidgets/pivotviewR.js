HTMLWidgets.widget({

  name: 'pivotviewR',

  type: 'output',

  factory: function(el, width, height) {

    return {

      renderValue: function(x) {

      x.Items = HTMLWidgets.dataframeToD3(x.Items);
      x.FacetCategories = HTMLWidgets.dataframeToD3(x.FacetCategories);

        $(el).PivotViewer({
          Loader: new PivotViewer.Models.Loaders.JSONLoader(x)
        });

      },

      resize: function(width, height) {

        $(el).PivotViewer('refresh');

      }

    };
  }
});
