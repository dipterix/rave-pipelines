const path = require('path');

module.exports = {
  entry: './src/index.js',
  output: {
    filename: 'shidashi.js',
    path: path.resolve(__dirname, 'www', 'shidashi', 'js'),
    libraryTarget: 'var',
    library: 'RAVEPipeline'
  },
  devtool: 'source-map',
  externals: {
    jquery: 'jQuery',
  },
  module: {
    rules: [
      {
        test: /\.(c|sa|sc)ss$/i,
        use: [
          'style-loader',
          'css-loader',
          'sass-loader'
        ]
      }
    ],
  },
};
