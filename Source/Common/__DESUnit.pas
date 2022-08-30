unit __DESUnit;

interface

uses
  SysUtils, Classes;

function __En__(aStr: string; aKey: string): string;
function __De__(aStr: string; aKey: string): string;

implementation

type
  TByte32 = array[1..32] of byte;
  TSData = array[0..63] of byte;
  TBlock = array[0..7] of byte;

const
  SA1                       : TSData =
    (1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1);
  SA2                       : TSData =
    (1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1);
  SA3                       : TSData =
    (1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1);
  SA4                       : TSData =
    (0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1);
  SA5                       : TSData =
    (0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0);
  SA6                       : TSData =
    (1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1);
  SA7                       : TSData =
    (0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1);
  SA8                       : TSData =
    (1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1);
  SB1                       : TSData =
    (1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1);
  SB2                       : TSData =
    (1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0);
  SB3                       : TSData =
    (0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1);
  SB4                       : TSData =
    (1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1);
  SB5                       : TSData =
    (0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0);
  SB6                       : TSData =
    (1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1);
  SB7                       : TSData =
    (1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1);
  SB8                       : TSData =
    (1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0);
  SC1                       : TSData =
    (1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0);
  SC2                       : TSData =
    (1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0);
  SC3                       : TSData =
    (1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0);
  SC4                       : TSData =
    (1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1);
  SC5                       : TSData =
    (1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1);
  SC6                       : TSData =
    (0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0);
  SC7                       : TSData =
    (0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0);
  SC8                       : TSData =
    (0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1);
  SD1                       : TSData =
    (0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1);
  SD2                       : TSData =
    (1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1);
  SD3                       : TSData =
    (0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0);
  SD4                       : TSData =
    (1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0);
  SD5                       : TSData =
    (1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1);
  SD6                       : TSData =
    (0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1);
  SD7                       : TSData =
    (0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0);
  SD8                       : TSData =
    (1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1);

  SC                        : array[1..16, 1..48] of byte =
    ((15, 18, 12, 25, 2, 6, 4, 1, 16, 7, 22, 11, 24, 20, 13, 5, 27, 9, 17, 8, 28, 21, 14, 3, 42, 53, 32, 38, 48, 56, 31, 41, 52, 46, 34, 49, 45, 50, 40, 29, 35, 54, 47, 43, 51, 37, 30, 33),
    (16, 19, 13, 26, 3, 7, 5, 2, 17, 8, 23, 12, 25, 21, 14, 6, 28, 10, 18, 9, 1, 22, 15, 4, 43, 54, 33, 39, 49, 29, 32, 42, 53, 47, 35, 50, 46, 51, 41, 30, 36, 55, 48, 44, 52, 38, 31, 34),
    (18, 21, 15, 28, 5, 9, 7, 4, 19, 10, 25, 14, 27, 23, 16, 8, 2, 12, 20, 11, 3, 24, 17, 6, 45, 56, 35, 41, 51, 31, 34, 44, 55, 49, 37, 52, 48, 53, 43, 32, 38, 29, 50, 46, 54, 40, 33, 36),
    (20, 23, 17, 2, 7, 11, 9, 6, 21, 12, 27, 16, 1, 25, 18, 10, 4, 14, 22, 13, 5, 26, 19, 8, 47, 30, 37, 43, 53, 33, 36, 46, 29, 51, 39, 54, 50, 55, 45, 34, 40, 31, 52, 48, 56, 42, 35, 38),
    (22, 25, 19, 4, 9, 13, 11, 8, 23, 14, 1, 18, 3, 27, 20, 12, 6, 16, 24, 15, 7, 28, 21, 10, 49, 32, 39, 45, 55, 35, 38, 48, 31, 53, 41, 56, 52, 29, 47, 36, 42, 33, 54, 50, 30, 44, 37, 40),
    (24, 27, 21, 6, 11, 15, 13, 10, 25, 16, 3, 20, 5, 1, 22, 14, 8, 18, 26, 17, 9, 2, 23, 12, 51, 34, 41, 47, 29, 37, 40, 50, 33, 55, 43, 30, 54, 31, 49, 38, 44, 35, 56, 52, 32, 46, 39, 42),
    (26, 1, 23, 8, 13, 17, 15, 12, 27, 18, 5, 22, 7, 3, 24, 16, 10, 20, 28, 19, 11, 4, 25, 14, 53, 36, 43, 49, 31, 39, 42, 52, 35, 29, 45, 32, 56, 33, 51, 40, 46, 37, 30, 54, 34, 48, 41, 44),
    (28, 3, 25, 10, 15, 19, 17, 14, 1, 20, 7, 24, 9, 5, 26, 18, 12, 22, 2, 21, 13, 6, 27, 16, 55, 38, 45, 51, 33, 41, 44, 54, 37, 31, 47, 34, 30, 35, 53, 42, 48, 39, 32, 56, 36, 50, 43, 46),
    (1, 4, 26, 11, 16, 20, 18, 15, 2, 21, 8, 25, 10, 6, 27, 19, 13, 23, 3, 22, 14, 7, 28, 17, 56, 39, 46, 52, 34, 42, 45, 55, 38, 32, 48, 35, 31, 36, 54, 43, 49, 40, 33, 29, 37, 51, 44, 47),
    (3, 6, 28, 13, 18, 22, 20, 17, 4, 23, 10, 27, 12, 8, 1, 21, 15, 25, 5, 24, 16, 9, 2, 19, 30, 41, 48, 54, 36, 44, 47, 29, 40, 34, 50, 37, 33, 38, 56, 45, 51, 42, 35, 31, 39, 53, 46, 49),
    (5, 8, 2, 15, 20, 24, 22, 19, 6, 25, 12, 1, 14, 10, 3, 23, 17, 27, 7, 26, 18, 11, 4, 21, 32, 43, 50, 56, 38, 46, 49, 31, 42, 36, 52, 39, 35, 40, 30, 47, 53, 44, 37, 33, 41, 55, 48, 51),
    (7, 10, 4, 17, 22, 26, 24, 21, 8, 27, 14, 3, 16, 12, 5, 25, 19, 1, 9, 28, 20, 13, 6, 23, 34, 45, 52, 30, 40, 48, 51, 33, 44, 38, 54, 41, 37, 42, 32, 49, 55, 46, 39, 35, 43, 29, 50, 53),
    (9, 12, 6, 19, 24, 28, 26, 23, 10, 1, 16, 5, 18, 14, 7, 27, 21, 3, 11, 2, 22, 15, 8, 25, 36, 47, 54, 32, 42, 50, 53, 35, 46, 40, 56, 43, 39, 44, 34, 51, 29, 48, 41, 37, 45, 31, 52, 55),
    (11, 14, 8, 21, 26, 2, 28, 25, 12, 3, 18, 7, 20, 16, 9, 1, 23, 5, 13, 4, 24, 17, 10, 27, 38, 49, 56, 34, 44, 52, 55, 37, 48, 42, 30, 45, 41, 46, 36, 53, 31, 50, 43, 39, 47, 33, 54, 29),
    (13, 16, 10, 23, 28, 4, 2, 27, 14, 5, 20, 9, 22, 18, 11, 3, 25, 7, 15, 6, 26, 19, 12, 1, 40, 51, 30, 36, 46, 54, 29, 39, 50, 44, 32, 47, 43, 48, 38, 55, 33, 52, 45, 41, 49, 35, 56, 31),
    (14, 17, 11, 24, 1, 5, 3, 28, 15, 6, 21, 10, 23, 19, 12, 4, 26, 8, 16, 7, 27, 20, 13, 2, 41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, 48, 44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32));

var
  g                         : array[1..16, 1..48] of byte;
  l, r, F                   : TByte32;
  c                         : array[1..56] of byte;

procedure DES_Init(Key: TBlock; FCode: Boolean);
var
  n, H                      : byte;
begin
  c[1] := Ord(Key[7] and 128 > 0);
  c[29] := Ord(Key[7] and 2 > 0);
  c[2] := Ord(Key[6] and 128 > 0);
  c[30] := Ord(Key[6] and 2 > 0);
  c[3] := Ord(Key[5] and 128 > 0);
  c[31] := Ord(Key[5] and 2 > 0);
  c[4] := Ord(Key[4] and 128 > 0);
  c[32] := Ord(Key[4] and 2 > 0);
  c[5] := Ord(Key[3] and 128 > 0);
  c[33] := Ord(Key[3] and 2 > 0);
  c[6] := Ord(Key[2] and 128 > 0);
  c[34] := Ord(Key[2] and 2 > 0);
  c[7] := Ord(Key[1] and 128 > 0);
  c[35] := Ord(Key[1] and 2 > 0);
  c[8] := Ord(Key[0] and 128 > 0);
  c[36] := Ord(Key[0] and 2 > 0);

  c[9] := Ord(Key[7] and 64 > 0);
  c[37] := Ord(Key[7] and 4 > 0);
  c[10] := Ord(Key[6] and 64 > 0);
  c[38] := Ord(Key[6] and 4 > 0);
  c[11] := Ord(Key[5] and 64 > 0);
  c[39] := Ord(Key[5] and 4 > 0);
  c[12] := Ord(Key[4] and 64 > 0);
  c[40] := Ord(Key[4] and 4 > 0);
  c[13] := Ord(Key[3] and 64 > 0);
  c[41] := Ord(Key[3] and 4 > 0);
  c[14] := Ord(Key[2] and 64 > 0);
  c[42] := Ord(Key[2] and 4 > 0);
  c[15] := Ord(Key[1] and 64 > 0);
  c[43] := Ord(Key[1] and 4 > 0);
  c[16] := Ord(Key[0] and 64 > 0);
  c[44] := Ord(Key[0] and 4 > 0);

  c[17] := Ord(Key[7] and 32 > 0);
  c[45] := Ord(Key[7] and 8 > 0);
  c[18] := Ord(Key[6] and 32 > 0);
  c[46] := Ord(Key[6] and 8 > 0);
  c[19] := Ord(Key[5] and 32 > 0);
  c[47] := Ord(Key[5] and 8 > 0);
  c[20] := Ord(Key[4] and 32 > 0);
  c[48] := Ord(Key[4] and 8 > 0);
  c[21] := Ord(Key[3] and 32 > 0);
  c[49] := Ord(Key[3] and 8 > 0);
  c[22] := Ord(Key[2] and 32 > 0);
  c[50] := Ord(Key[2] and 8 > 0);
  c[23] := Ord(Key[1] and 32 > 0);
  c[51] := Ord(Key[1] and 8 > 0);
  c[24] := Ord(Key[0] and 32 > 0);
  c[52] := Ord(Key[0] and 8 > 0);

  c[25] := Ord(Key[7] and 16 > 0);
  c[53] := Ord(Key[3] and 16 > 0);
  c[26] := Ord(Key[6] and 16 > 0);
  c[54] := Ord(Key[2] and 16 > 0);
  c[27] := Ord(Key[5] and 16 > 0);
  c[55] := Ord(Key[1] and 16 > 0);
  c[28] := Ord(Key[4] and 16 > 0);
  c[56] := Ord(Key[0] and 16 > 0);

  if FCode then begin
    for n := 1 to 16 do begin
      for H := 1 to 48 do
        g[n, H] := c[SC[n, H]];
    end;
  end else begin
    for n := 1 to 16 do begin
      for H := 1 to 48 do
        g[17 - n, H] := c[SC[n, H]];
    end;
  end;
end;

procedure DES_Code(Input: TBlock; var Output: TBlock);
var
  n                         : byte;
  z                         : Word;
begin
  l[1] := Ord(Input[7] and 64 > 0);
  r[1] := Ord(Input[7] and 128 > 0);
  l[2] := Ord(Input[6] and 64 > 0);
  r[2] := Ord(Input[6] and 128 > 0);
  l[3] := Ord(Input[5] and 64 > 0);
  r[3] := Ord(Input[5] and 128 > 0);
  l[4] := Ord(Input[4] and 64 > 0);
  r[4] := Ord(Input[4] and 128 > 0);
  l[5] := Ord(Input[3] and 64 > 0);
  r[5] := Ord(Input[3] and 128 > 0);
  l[6] := Ord(Input[2] and 64 > 0);
  r[6] := Ord(Input[2] and 128 > 0);
  l[7] := Ord(Input[1] and 64 > 0);
  r[7] := Ord(Input[1] and 128 > 0);
  l[8] := Ord(Input[0] and 64 > 0);
  r[8] := Ord(Input[0] and 128 > 0);
  l[9] := Ord(Input[7] and 16 > 0);
  r[9] := Ord(Input[7] and 32 > 0);
  l[10] := Ord(Input[6] and 16 > 0);
  r[10] := Ord(Input[6] and 32 > 0);
  l[11] := Ord(Input[5] and 16 > 0);
  r[11] := Ord(Input[5] and 32 > 0);
  l[12] := Ord(Input[4] and 16 > 0);
  r[12] := Ord(Input[4] and 32 > 0);
  l[13] := Ord(Input[3] and 16 > 0);
  r[13] := Ord(Input[3] and 32 > 0);
  l[14] := Ord(Input[2] and 16 > 0);
  r[14] := Ord(Input[2] and 32 > 0);
  l[15] := Ord(Input[1] and 16 > 0);
  r[15] := Ord(Input[1] and 32 > 0);
  l[16] := Ord(Input[0] and 16 > 0);
  r[16] := Ord(Input[0] and 32 > 0);
  l[17] := Ord(Input[7] and 4 > 0);
  r[17] := Ord(Input[7] and 8 > 0);
  l[18] := Ord(Input[6] and 4 > 0);
  r[18] := Ord(Input[6] and 8 > 0);
  l[19] := Ord(Input[5] and 4 > 0);
  r[19] := Ord(Input[5] and 8 > 0);
  l[20] := Ord(Input[4] and 4 > 0);
  r[20] := Ord(Input[4] and 8 > 0);
  l[21] := Ord(Input[3] and 4 > 0);
  r[21] := Ord(Input[3] and 8 > 0);
  l[22] := Ord(Input[2] and 4 > 0);
  r[22] := Ord(Input[2] and 8 > 0);
  l[23] := Ord(Input[1] and 4 > 0);
  r[23] := Ord(Input[1] and 8 > 0);
  l[24] := Ord(Input[0] and 4 > 0);
  r[24] := Ord(Input[0] and 8 > 0);
  l[25] := Input[7] and 1;
  r[25] := Ord(Input[7] and 2 > 0);
  l[26] := Input[6] and 1;
  r[26] := Ord(Input[6] and 2 > 0);
  l[27] := Input[5] and 1;
  r[27] := Ord(Input[5] and 2 > 0);
  l[28] := Input[4] and 1;
  r[28] := Ord(Input[4] and 2 > 0);
  l[29] := Input[3] and 1;
  r[29] := Ord(Input[3] and 2 > 0);
  l[30] := Input[2] and 1;
  r[30] := Ord(Input[2] and 2 > 0);
  l[31] := Input[1] and 1;
  r[31] := Ord(Input[1] and 2 > 0);
  l[32] := Input[0] and 1;
  r[32] := Ord(Input[0] and 2 > 0);

  for n := 1 to 16 do begin
    z := ((r[32] xor g[n, 1]) shl 5) or ((r[5] xor g[n, 6]) shl 4) or ((r[1] xor g[n, 2]) shl 3) or ((r[2] xor g[n, 3]) shl 2) or ((r[3] xor g[n, 4]) shl 1) or (r[4] xor g[n, 5]);
    F[9] := l[9] xor SA1[z];
    F[17] := l[17] xor SB1[z];
    F[23] := l[23] xor SC1[z];
    F[31] := l[31] xor SD1[z];

    z := ((r[4] xor g[n, 7]) shl 5) or ((r[9] xor g[n, 12]) shl 4) or ((r[5] xor g[n, 8]) shl 3) or ((r[6] xor g[n, 9]) shl 2) or ((r[7] xor g[n, 10]) shl 1) or (r[8] xor g[n, 11]);
    F[13] := l[13] xor SA2[z];
    F[28] := l[28] xor SB2[z];
    F[2] := l[2] xor SC2[z];
    F[18] := l[18] xor SD2[z];

    z := ((r[8] xor g[n, 13]) shl 5) or ((r[13] xor g[n, 18]) shl 4) or ((r[9] xor g[n, 14]) shl 3) or ((r[10] xor g[n, 15]) shl 2) or ((r[11] xor g[n, 16]) shl 1) or (r[12] xor g[n, 17]);
    F[24] := l[24] xor SA3[z];
    F[16] := l[16] xor SB3[z];
    F[30] := l[30] xor SC3[z];
    F[6] := l[6] xor SD3[z];

    z := ((r[12] xor g[n, 19]) shl 5) or ((r[17] xor g[n, 24]) shl 4) or ((r[13] xor g[n, 20]) shl 3) or ((r[14] xor g[n, 21]) shl 2) or ((r[15] xor g[n, 22]) shl 1) or (r[16] xor g[n, 23]);
    F[26] := l[26] xor SA4[z];
    F[20] := l[20] xor SB4[z];
    F[10] := l[10] xor SC4[z];
    F[1] := l[1] xor SD4[z];

    z := ((r[16] xor g[n, 25]) shl 5) or ((r[21] xor g[n, 30]) shl 4) or ((r[17] xor g[n, 26]) shl 3) or ((r[18] xor g[n, 27]) shl 2) or ((r[19] xor g[n, 28]) shl 1) or (r[20] xor g[n, 29]);
    F[8] := l[8] xor SA5[z];
    F[14] := l[14] xor SB5[z];
    F[25] := l[25] xor SC5[z];
    F[3] := l[3] xor SD5[z];

    z := ((r[20] xor g[n, 31]) shl 5) or ((r[25] xor g[n, 36]) shl 4) or ((r[21] xor g[n, 32]) shl 3) or ((r[22] xor g[n, 33]) shl 2) or ((r[23] xor g[n, 34]) shl 1) or (r[24] xor g[n, 35]);
    F[4] := l[4] xor SA6[z];
    F[29] := l[29] xor SB6[z];
    F[11] := l[11] xor SC6[z];
    F[19] := l[19] xor SD6[z];

    z := ((r[24] xor g[n, 37]) shl 5) or ((r[29] xor g[n, 42]) shl 4) or ((r[25] xor g[n, 38]) shl 3) or ((r[26] xor g[n, 39]) shl 2) or ((r[27] xor g[n, 40]) shl 1) or (r[28] xor g[n, 41]);
    F[32] := l[32] xor SA7[z];
    F[12] := l[12] xor SB7[z];
    F[22] := l[22] xor SC7[z];
    F[7] := l[7] xor SD7[z];

    z := ((r[28] xor g[n, 43]) shl 5) or ((r[1] xor g[n, 48]) shl 4) or ((r[29] xor g[n, 44]) shl 3) or ((r[30] xor g[n, 45]) shl 2) or ((r[31] xor g[n, 46]) shl 1) or (r[32] xor g[n, 47]);
    F[5] := l[5] xor SA8[z];
    F[27] := l[27] xor SB8[z];
    F[15] := l[15] xor SC8[z];
    F[21] := l[21] xor SD8[z];

    l := r;
    r := F;
  end;

  Output[0] := (l[8] shl 7) or (r[8] shl 6) or (l[16] shl 5) or (r[16] shl 4) or (l[24] shl 3) or (r[24] shl 2) or (l[32] shl 1) or r[32];
  Output[1] := (l[7] shl 7) or (r[7] shl 6) or (l[15] shl 5) or (r[15] shl 4) or (l[23] shl 3) or (r[23] shl 2) or (l[31] shl 1) or r[31];
  Output[2] := (l[6] shl 7) or (r[6] shl 6) or (l[14] shl 5) or (r[14] shl 4) or (l[22] shl 3) or (r[22] shl 2) or (l[30] shl 1) or r[30];
  Output[3] := (l[5] shl 7) or (r[5] shl 6) or (l[13] shl 5) or (r[13] shl 4) or (l[21] shl 3) or (r[21] shl 2) or (l[29] shl 1) or r[29];
  Output[4] := (l[4] shl 7) or (r[4] shl 6) or (l[12] shl 5) or (r[12] shl 4) or (l[20] shl 3) or (r[20] shl 2) or (l[28] shl 1) or r[28];
  Output[5] := (l[3] shl 7) or (r[3] shl 6) or (l[11] shl 5) or (r[11] shl 4) or (l[19] shl 3) or (r[19] shl 2) or (l[27] shl 1) or r[27];
  Output[6] := (l[2] shl 7) or (r[2] shl 6) or (l[10] shl 5) or (r[10] shl 4) or (l[18] shl 3) or (r[18] shl 2) or (l[26] shl 1) or r[26];
  Output[7] := (l[1] shl 7) or (r[1] shl 6) or (l[9] shl 5) or (r[9] shl 4) or (l[17] shl 3) or (r[17] shl 2) or (l[25] shl 1) or r[25];
end;

function StrToKey(aKey: string): TBlock;
var
  Key                       : TBlock;
  i                         : Integer;
begin
  FillChar(Key, SizeOf(TBlock), 0);
  for i := 1 to Length(aKey) do
    Key[i mod SizeOf(TBlock)] := Key[i mod SizeOf(TBlock)] + Ord(aKey[i]);
  Result := Key;
end;

function __En__(aStr: string; aKey: string): string;
var
  ReadBuf                   : TBlock;
  WriteBuf                  : TBlock;
  Key                       : TBlock;
  Count                     : Integer;
  Offset                    : Integer;
  i                         : Integer;
  s, sS                     : string;
begin
  Result := '';
  //aKey := g_key;
  Key := StrToKey(aKey);
  DES_Init(Key, True);

  Offset := 1;
  Count := Length(aStr);
  repeat
    s := Copy(aStr, Offset, 8);
    FillChar(ReadBuf, 8, 0);
    Move(s[1], ReadBuf, Length(s));
    DES_Code(ReadBuf, WriteBuf);
    for i := 0 to 7 do
      sS := sS + IntToHex(WriteBuf[i], 2);
    Offset := Offset + 8;
  until Offset > ((Count + 7) div 8) * 8;
  Result := PChar(sS);
end;

function __De__(aStr: string; aKey: string): string;
var
  ReadBuf                   : TBlock;
  WriteBuf                  : TBlock;
  Key                       : TBlock;
  Offset                    : Integer;
  Count                     : Integer;
  i                         : Integer;
  s, sS {, aKey}            : string;
begin
  Result := '';
  try
    Key := StrToKey(aKey);
    DES_Init(Key, False);
    s := '';
    i := 1;
    repeat
      s := s + Chr(StrToInt('$' + Copy(aStr, i, 2)));
      Inc(i, 2);
    until i > Length(aStr);

    Offset := 1;
    Count := Length(s);
    while Offset < ((Count + 7) div 8 * 8) do begin
      FillChar(ReadBuf, 8, 0);
      Move(s[Offset], ReadBuf, 8);
      DES_Code(ReadBuf, WriteBuf);
      for i := 0 to 7 do
        sS := sS + Chr(WriteBuf[i]);
      Offset := Offset + 8;
    end;
    Result := PChar(sS);
  except
    Result := '';
  end;
end;

//加密8字节的数据

function EnCrypt(in_Buf: Tblock; aKey: string): TBlock;
var
  WriteBuf                  : TBlock;
  Key                       : TBlock;
begin
  try
    fillchar(writeBuf, 8, 0);
    Key := StrToKey(aKey);
    Des_Init(Key, True);
    Des_Code(in_buf, WriteBuf);
    result := writebuf;
  except
    fillchar(writeBuf, 8, 0);
    result := writebuf;
  end;
end;

//解密8字节的数据

function DeCrypt(in_buf: Tblock; aKey: string): Tblock;
var
  WriteBuf                  : TBlock;
  Key                       : TBlock;
begin
  try
    Key := StrToKey(aKey);
    Des_Init(Key, False);
    FillChar(WriteBuf, 8, 0);
    Des_Code(in_Buf, WriteBuf);
    result := WriteBuf;
  except
    FillChar(WriteBuf, 8, 0);
    result := WriteBuf;
  end;
end;

procedure __EnCryptFile(infile, outfile: string; aKey: string);
var
  ReadBuf                   : TBlock;
  WriteBuf                  : TBlock;
  LEN, len1, len2, I        : INTEGER;
  ms1, ms2                  : TMemorystream;
begin
  if (infile = '') or (outfile = '') or (akey = '') then exit;
  ms1 := nil;
  ms2 := nil;
  try
    ms1 := TMemorystream.Create;
    ms2 := TMemorystream.Create;
    ms1.LoadFromFile(infile);
    //得到原始文件长度并按8的整数倍对齐
    len := ms1.Size;
    len1 := (len + 7) shr 3;            //  按 8Bytes(64bits) 对齐
    ms1.Size := len1 shl 3;

    len2 := ms1.Size;
    ms1.Position := 0;
    len1 := len2 div 8;
    for i := 0 to len1 - 1 do begin
      ms1.Read(Readbuf, 8);
      writebuf := EnCrypt(readbuf, aKey);
      ms2.Write(writebuf, 8);
    end;
    //保存原始文件长度
    ms2.Seek(0, soFromEnd);
    ms2.Write(len, SizeOf(len));        //  将原始大小记录在数据末
    ms2.SaveToFile(outfile);
    ms1.Free;
    ms2.Free;
  except
    ms1.Free;
    ms2.Free;
  end;
end;

//解密文件

procedure __DeCryptFile(infile, outfile: string; aKey: string);
var
  ReadBuf,
    WriteBuf                : TBlock;
  LEN, len1, I              : INTEGER;
  ms1, ms2                  : TMemorystream;
begin
  if (infile = '') or (outfile = '') or (akey = '') then exit;
  ms1 := nil;
  ms2 := nil;
  try
    //加密
    ms1 := TMemorystream.Create;
    ms2 := TMemorystream.Create;
    ms1.LoadFromFile(infile);
    //得到原始文件长度len
    len1 := SizeOf(len);
    ms1.Seek(-len1, soFromEnd);         //  取得原始大小
    ms1.Read(len, len1);

    len1 := ms1.Size - len1;
    ms1.Size := len1;
    len1 := (len1 + 7) shr 3;           //  按 8Bytes(64bits) 对齐
    if (ms1.Size <> (len1 shl 3)) then
      raise Exception.Create('Invalid data size!');

    len1 := ms1.Size;
    len1 := len1 div 8;
    ms1.Position := 0;
    ms2.Position := 0;
    fillchar(readbuf, 8, 0);
    for i := 0 to len1 - 1 do begin
      ms1.Read(readbuf, 8);
      writebuf := DeCrypt(readbuf, aKey);
      ms2.Write(writebuf, 8);
    end;
    ms2.SetSize(len);
    ms2.SaveToFile(outfile);
    ms1.Free;
    ms2.Free;
  except
    ms1.Free;
    ms2.Free;
  end;
end;

end.

