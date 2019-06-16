import { Component, OnInit } from '@angular/core';
import { CartService, Product } from '../cart.service';
import { FormBuilder } from '@angular/forms';

@Component({
  selector: 'app-cart',
  templateUrl: './cart.component.html',
  styleUrls: ['./cart.component.css']
})
export class CartComponent implements OnInit {
  items: Product[];
  checkoutForm;

  constructor(
    private cartService: CartService,
    private formBuilder: FormBuilder
  ) {
    this.items = this.cartService.getItems();

    this.checkoutForm = this.formBuilder.group({
      name: '',
      address: ''
    })
   }

  ngOnInit() {
  }

  onSubmit(customerData: any) {
    console.warn('Your order has been submitted ', customerData);

    this.items = this.cartService.clearCart();
    this.checkoutForm.reset();
  }

}
