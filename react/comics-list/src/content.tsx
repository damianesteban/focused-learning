import * as React from 'react';
import { useLocalStore } from 'mobx-react-lite';
import { createStore, Store } from './store';

export const storeContext = React.createContext<Store | null>(null);

export const StoreProvider: React.FC = ({ children }) => {
  const store = useLocalStore(createStore);

  return (
    <storeContext.Provider value={store}>
      {children}
    </storeContext.Provider>
  );
};

export default StoreProvider;
