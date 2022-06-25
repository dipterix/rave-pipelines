# Please install node.js npm first

## Install admin-lte
# npm install admin-lte@^3.2 --save

cd node_modules/admin-lte/

## Install dependencies for admin-lte
# npm install --legacy-peer-deps

# npm run compile
# npm run production

cp -rf dist/js/adminlte.j* ../../www/AdminLTE/js
cp -rf dist/css/adminlte.cs* ../../www/AdminLTE/css

cd ../../

# npm install node-sass --save-dev
# npm install sass-loader --save-dev
# npm install css-loader --save-dev
# npm install style-loader --save-dev
# npm install webpack webpack-cli --save-dev
# npm install clipboard --save
# npm install overlayscrollbars --save
npm install exports-loader --save-dev
