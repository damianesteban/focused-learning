import React from 'react';
import ComicsList from './components/ComicsList';
import StoreProvider from './content';
import Search from './components/Search';
import { Box, Button, Heading, Grommet, Stack } from 'grommet';
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
          <Stack anchor='center'>
            <Box flex align='center' justify='center'>
              <Box 
                align='center' 
                justify='center'
                height='medium'
                pad='large'
              >
                <Heading level={3}>
                  Comic Book Search
                </Heading>
                <Search />
                <ComicsList />
              </Box>
            </Box>
            </Stack>
           
          </Box>
          </Box>
      </Grommet>
    </StoreProvider>
  );
}

export default App;
