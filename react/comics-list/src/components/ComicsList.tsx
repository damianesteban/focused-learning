import * as React from 'react';
import { storeContext } from '../content';
import { useObserver } from 'mobx-react-lite';
import { Box, Button, Collapsible, DataTable, Heading, Grommet, ResponsiveContext, Text } from 'grommet';
import { theme } from '../App';

export const ComicView: React.FC<{ comics: string []}> =
  ({ comics }) => {
    return (
      <ul>
        {comics.map(comic => <li>{comic}</li>)}
      </ul>
    );
}

const columns = [
  {
    property: 'title',
    header: <Text>Comic Title</Text>,
    primary: true
  }
]

export const ComicsList = () => {
  const store = React.useContext(storeContext);
  if (!store) throw Error('store should not be null');
  return useObserver(() => {
    return (
      <Grommet theme={theme}>
        <Box align="center" pad="xsmall">
          <DataTable columns={columns} data={store.filteredComics} step={10} />
        </Box>
      </Grommet>
    )
  })
}

export default ComicsList;
