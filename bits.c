/* * CS:APP Data Lab * * <Please put your name and userid here> *Name: Alvie Thai * TCU ID: 110424342 * bits.c - 
 Source file with your solutions to the Lab. * This is the file you will hand in to your instructor. * * 
 WARNING: Do not include the <stdio.h> header; it confuses the dlc * compiler. You can still use printf for 
 debugging without including * <stdio.h>, although you might get a compiler warning. In general, * it's not good 
 practice to ignore compiler warnings, but in this * case it's OK.  */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31.


EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implement floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants. You can use any arithmetic,
logical, or comparison operations on int or unsigned data.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.  The max operator count is checked by dlc.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2022 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
// 1
/* 
 * bitMatch - Create mask indicating which bits in x match those in y
 *            using only ~ and & 
 *   Example: bitMatch(0x7, 0xE) = 0x6
 *   Legal ops: ~ & |
 *   Max ops: 14
 *   Rating: 1
 */
int bitMatch(int x, int y) {
  int matchedOnes=x&y;//match the 1
  int matchedZero=~x&~y;//return 1 if the zero location matchh

  return ~(~matchedOnes&~matchedZero);//(matchedOnes|matched Zero)
}
/* 
 * bitXor - x^y using only ~ and & 
 *   Example: bitXor(4, 5) = 1
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */
int bitXor(int x, int y) {
  int notX=(~x&y);//match to see if there is any 0 in x match with 1 in y
  int notY=(x&~y);//check if there is any 0 in y match 1 in x
  return ~((~notX)&(~notY));
}
/* 
 * minusOne - return a value of -1 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 2
 *   Rating: 1
 */
int minusOne(void) {
  int flip=~(0x1);//flip and add 1
  int add=flip+1;
  return add;
}
/* 
 * specialBits - return bit pattern 0xffca3fff
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 3
 *   Rating: 1
 */
int specialBits(void) {//we know that the pattern has 1111...10010100011111.., so we flip 0010 into 1101=D and flip 
//1000 into 0111 and then move into the appropriate position and then negate them all
    return ~(0xD7 <<14);
}
/* 
 * TMax - return maximum two's complement integer 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmax(void) {
  int eight=0x8;//1000
  return ~(eight<<28);//01111=2^31-1
}
//2
/* 
 * anyEvenBit - return 1 if any even-numbered bit in word set to 1
 *   where bits are numbered from 0 (least significant) to 31 (most significant)
 *   Examples anyEvenBit(0xA) = 0, anyEvenBit(0xE) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int anyEvenBit(int x) {
  	int evenSet=0x55;//in binary it's 0101 0101
	int k=x&evenSet;// return 0 if there's no even bit
        evenSet=evenSet<<8;//move them into the each byte and or them all
	k=k+(x&evenSet);
	evenSet=evenSet<<8;
	k=k+(x&evenSet);
	evenSet=evenSet<<8;
	k=k+(x&evenSet);
	return !!k;
}
/* 
 * byteSwap - swaps the nth byte and the mth byte
 *  Examples: byteSwap(0x12345678, 1, 3) = 0x56341278
 *            byteSwap(0xDEADBEEF, 0, 2) = 0xDEEFBEAD
 *  You may assume that 0 <= n <= 3, 0 <= m <= 3
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 2
 */
int byteSwap(int x, int n, int m) {
        int one=0xFF;
	int y=0;
	int leftB=(x>>(n<<3));//multiple n by 8, then right shift x 8n bits, to put the n byte at the left most place
	int rightB=(x>>(m<<3));
	y=one&(leftB^rightB);//get 1 if the bits are different, otherwise 0
	x=x^(y<<(n<<3));
	x=x^(y<<(m<<3));
	return x;
}
/* 
 * dividePower2 - Compute x/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   Examples: dividePower2(15,1) = 7, dividePower2(-33,4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int dividePower2(int x, int n) {
   int sign=x>>31;// calulate the sign bit
   int bias=(1<<n)+~0;
   int adj_x=(x+(sign&bias))>>n;//plus the bias than divide by 2^n
   return adj_x;
}
/* 
 * floatNegate - Return bit-level equivalent of expression -f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned floatNegate(unsigned uf) {
 	unsigned po=(uf>>23)&0xFF;//get the last 2 byte
	unsigned frac=uf<<9;//emit the last 9 bit
	if(po==0xFF&&frac!=0x00){
		return uf;//return the argument if it is NaN
	}return uf^(1<<31);
	
}
/* 
 * getByte - Extract byte n from word x
 *   Bytes numbered from 0 (least significant) to 3 (most significant)
 *   Examples: getByte(0x12345678,1) = 0x56
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int getByte(int x, int n) {
  int k=0xFF;
  int h=k<<(n<<3);//shift the 11111111 to the 8*n bit
  int a=x&h;//get that byte
  return (a>>(n<<3))&k;//shift them into the first byte
}
/* 
 * isEqual - return 1 if x == y, and 0 otherwise 
 *   Examples: isEqual(5,5) = 1, isEqual(4,5) = 0
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int isEqual(int x, int y) {
	int  checkZ=x^y;//return 1  if there is any different bit
	
  	return !(checkZ);
}
//3
/* 
 * addOK - Determine if can compute x+y without overflow
 *   Example: addOK(0x80000000,0x80000000) = 0,
 *            addOK(0x80000000,0x70000000) = 1, 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int addOK(int x, int y) {
	int sum=x+y;//overflow happens when x and y have different sign bit AND sum is the opposite of x and
	int sumS=(sum>>31)&1;//return 1 if sign bit is 1
	int xS=(x>>31)&1;
	int yS=(y>>31)&1;
  return !((!(xS^yS))&((sumS^xS)));
}
/* 
 * bitMask - Generate a mask consisting of all 1's 
 *   lowbit and highbit
 *   Examples: bitMask(5,3) = 0x38
 *   Assume 0 <= lowbit <= 31, and 0 <= highbit <= 31
 *   If lowbit > highbit, then mask should be all 0's
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int bitMask(int highbit, int lowbit) {
  int negOne=~0;
  lowbit=(negOne<<lowbit);
  highbit=(negOne<<highbit)<<1;
  return (lowbit^highbit)&lowbit;//clear everything after the lowbit
}
/*
 * ezThreeFourths - multiplies by 3/4 rounding toward 0,
 *   Should exactly duplicate effect of C expression (x*3/4),
 *   including overflow behavior.
 *   Examples: ezThreeFourths(11) = 8
 *             ezThreeFourths(-9) = -6
 *             ezThreeFourths(1073741824) = -268435456 (overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 3
 */
int ezThreeFourths(int x) {
  int mask;
  int  sign;
  int bias;
  int  Fdiv;
  x=((x<<1)+x);//multiply by 3
  mask=0x3;//0011
  sign=(x>>31);//get 1111... if negative otherwise 000..
  bias=mask&sign;//get 11 if negative or 00 if positive
  Fdiv=((x+bias)>>2);//add the bias and then divide by 4
  return Fdiv;
}
/* 
 * isGreater - if x > y  then return 1, else return 0 
 *   Example: isGreater(4,5) = 0, isGreater(5,4) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isGreater(int x, int y) {
  int signX=(x>>31)&1;//return 1 if sign is negative
  int signY=(y>>31)&1;
  int isD=signX^signY;//return 1 if they are different
  int signD=(isD)&(!signX);//if signX and signY are different, that x>y iff signX>0 
  int dSign=(((x+(~y+1))>>31)&1);//return 1 if x-y<0
  return  (signD|((!isD)&(!dSign)))&(!!(x^y));//return if either signD or dSign is true, and check whether x and y are equal
}
/* 
 * replaceByte(x,n,c) - Replace byte n in x with c
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: replaceByte(0x12345678,1,0xab) = 0x1234ab78
 *   You can assume 0 <= n <= 3 and 0 <= c <= 255
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 3
 */
int replaceByte(int x, int n, int c) {
  int mask=0xFF;
  int shift=n<<3;//shift to the n byte
   mask=~(mask<<shift);//extract the n byte
   c=c<<shift;//shift the c to the n byte
  return (x&mask)|c; //clear the byte at the n position and replace with c by or
}
//4
/* 
 * bang - Compute !x without using !
 *   Examples: bang(3) = 0, bang(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int bang(int x) {
  int negative=~x+1;//flip to get the negative
   int sign=(x|negative)>>31;//do x|(-x) to see the sign
  return sign+1;
}
/*
 * bitParity - returns 1 if x contains an odd number of 0's
 *   Examples: bitParity(5) = 0, bitParity(7) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int bitParity(int x) {
  x=(x>>16)^x;//get the first 16 bit and the difference bit between x and the first 16 bit of x
  x=(x>>8)^x;
  x=(x>>4)^x;
  x=(x>>2)^x;
  x=(x>>1)^x;
  return  0x01&x;
}
/* howManyBits - return the minimum number of bits required to represent x in
 *             two's complement
 *  Examples: howManyBits(12) = 5
 *            howManyBits(298) = 10
 *            howManyBits(-5) = 4
 *            howManyBits(0)  = 1
 *            howManyBits(-1) = 1
 *            howManyBits(0x80000000) = 32
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 90
 *  Rating: 4
 */
int howManyBits(int x) {
	int signB;
	int sum;
	int bias;
	signB=x>>31; // return 1111.. if negative otherwise 00000...
	x=signB^x;
	sum=(!!(x>>16))<<4;
	sum=sum|(!!(x>>(sum+8)))<<3;
	sum=sum|(!!(x>>(sum+4)))<<2;
	sum=sum|(!!(x>>(sum+2)))<<1;
	sum=sum|x>>(sum+1);
	bias=!(x^0); //return 1 if x is 0
	return sum +2 +(~bias+1);
}
/*
 * isPallindrome - Return 1 if bit pattern in x is equal to its mirror image
 *   Example: isPallindrome(0x01234567E6AC2480) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 45
 *   Rating: 4
 */
int isPallindrome(int x) {
	 int firstHalf = x >> 16;//get first 16 bit
  int mask = (0x55 << 8)|(0x55);//get 01010101...16 bit sequence
  firstHalf = ((firstHalf & mask) << 1) | ((firstHalf >> 1) & mask);
  mask = (0x33 << 8)|(0x33);//get 00110011... 16 bit sequemce
  firstHalf= ((firstHalf & mask) << 2) | ((firstHalf >> 2) & mask);
  mask = (0x0f << 8)|(0x0f);//00001111...
  firstHalf= ((firstHalf & mask) << 4) | ((firstHalf >> 4) & mask);
  firstHalf = ((firstHalf& 0xff) << 8) | ((firstHalf >> 8) & 0xff);

  return !((firstHalf ^ x) & (~0 + (1 << 16)));
}
/*
 * isPower2 - returns 1 if x is a power of 2, and 0 otherwise
 *   Examples: isPower2(5) = 0, isPower2(8) = 1, isPower2(0) = 0
 *   Note that no negative number is a power of 2.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int isPower2(int x) {
  int isNegative=(x>>31)&1;
  int isZero=(x^0);//return 0 if is  zero
  int oneB=(~(x&(~x+1))|(0x01<<31))&x;
  return ((!isNegative)&(!oneB)&(!!isZero));//check wheter x>0 and have exactly one 1 bit

}
/* 
 * signMag2TwosComp - Convert from sign-magnitude to two's complement
 *   where the MSB is the sign bit
 *   Example: signMag2TwosComp(0x80000005) = -5.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 4
 */
int signMag2TwosComp(int x) {
	int k=0x8;//we need 0x7FFFFFFF, so we find the their negate first
	int flip;
	int sign;
	k=~(k<<28);
	sign=(x>>31);//return 111... if is negative,000 if positive
	x=x&k;//emit the sign bit

	flip=(~x)+1;

	//if is negative, than flip, otherwise give 000..
      // if is positive than take x, otherwise give 000...

 	 return (~sign&x)|(sign&flip);
}
