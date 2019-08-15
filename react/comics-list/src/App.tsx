import React from 'react';
import ComicsList from './components/ComicsList';
import StoreProvider from './content';
import Search from './components/Search';
import { Box, Button, Collapsible, Heading, Grommet, ResponsiveContext } from 'grommet';
import { Notification } from 'grommet-icons';

export const theme = {
  global: {
    colors: {
      brand: '#228BE6',
    },
    font: {
      family: 'Roboto',
      size: '14px',
      height: '20px',
    },
  },
};

const AppBar = (props: any) => (
  <Box
    tag='header'
    direction='row'
    align='center'
    justify='between'
    background='brand'
    pad={{ left: 'medium', right: 'small', vertical: 'small' }}
    elevation='medium'
    style={{ zIndex: '1'}}
    {...props}
  />
)

const App: React.FC = () => {

  const [showSidebar, setSideBar] = React.useState(false);

  return (
    <StoreProvider>
      <Grommet theme={theme} full={true}>
        <Box fill>
          <AppBar>
            <Heading level='3' margin='none'>My App</Heading>
            <Button 
              icon={<Notification />}
              onClick={() => setSideBar(!showSidebar)}
            />
          </AppBar>
          <Box direction='row' flex overflow={{ horizontal: 'hidden' }}>
            <Box flex align='center' justify='center'>
              <Box 
                align='center' 
                justify='center'
                height='medium'
              >
                <Search />
                <ComicsList />
              </Box>
            </Box>
            {/* {showSidebar && ( */}
              <Collapsible direction="horizontal" open={showSidebar}>
             <Box
              width='medium'
              background='light-2'
              elevation='small'
              align='center'
              justify='center'
            >
            {/* )} */}
              sidebar
            </Box>
            </Collapsible>
          </Box>
          </Box>
      </Grommet>
    </StoreProvider>
  );
}

export default App;
