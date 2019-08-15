import React from 'react';
import { useObserver } from 'mobx-react-lite';
import { storeContext } from '../content';
import { Box, Grommet, TextInput } from 'grommet';
import { theme } from '../App';

const Search: React.FC = () => {
  const store = React.useContext(storeContext);
  if (!store) throw Error('store should not be null');

  const { query, setQuery } = store;

  return useObserver(() => {
    return (
      <Grommet full theme={theme}>
        <Box fill align="center" justify="start" pad="xsmall">
        <Box width="medium">
            <TextInput value={query.get()} onChange={e => setQuery(e.target.value)} />
          </Box>
        </Box>
      </Grommet>
    )
  })
}

export default Search;
