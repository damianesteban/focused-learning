import { observable } from "mobx";

type Comic = { title: string };

const comics = [
  { title: 'Detective Comics #10' },
  { title: 'Action Comics #1' },
  { title: 'Avengers #1' }
]

export const createStore = () => {
  const store = {
    query: observable.box(''), 
    setQuery(query: string) {
      store.query.set(query.toLowerCase())
    },
    get filteredComics() {
      return comics.filter((comic: Comic) =>
        comic.title.toLowerCase().includes(store.query.get())
      );
    }
  }
  return store;
}

export type Store = ReturnType<typeof createStore>;
