import { Injectable } from '@angular/core';
import { Hero } from './models/hero';
import { HEROES } from './models/mock-heroes';
import { Observable, of } from 'rxjs';
import { MessageService } from './message.service';
import { tap } from 'rxjs/operators';
@Injectable({
  providedIn: 'root'
})
export class HeroService {

  constructor(private messageService: MessageService) { }

  getHeroes(): Observable<Hero[]> {
    return of(HEROES).pipe(
      tap(_ => this.messageService.add('Hero Service: Fetched heroes'))
    );
  }

  getHero(id: number): Observable<Hero> {
    return of(HEROES.find(hero => hero.id === id)).pipe(
      tap(_ => this.messageService.add(`HeroService: fetched hero id=${id}`))
    )
  }
}
