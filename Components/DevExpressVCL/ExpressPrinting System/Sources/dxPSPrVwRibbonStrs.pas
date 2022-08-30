{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxPSPrVwRibbonStrs;

interface

{$I cxVer.inc}

resourcestring
  sdxColorBlack = 'Black';
  sdxColorDarkRed = 'Dark Red';
  sdxColorRed = 'Red';
  sdxColorPink = 'Pink';
  sdxColorRose = 'Rose';
  sdxColorBrown = 'Brown';
  sdxColorOrange = 'Orange';
  sdxColorLightOrange = 'Light Orange';
  sdxColorGold = 'Gold';
  sdxColorTan = 'Tan';
  sdxColorOliveGreen = 'Olive Green';
  sdxColorDrakYellow = 'Dark Yellow';
  sdxColorLime = 'Lime';
  sdxColorYellow = 'Yellow';
  sdxColorLightYellow = 'Light Yellow';
  sdxColorDarkGreen = 'Dark Green';
  sdxColorGreen = 'Green';
  sdxColorSeaGreen = 'Sea Green';
  sdxColorBrighthGreen = 'Bright Green';
  sdxColorLightGreen = 'Light Green';
  sdxColorDarkTeal = 'Dark Teal';
  sdxColorTeal = 'Teal';
  sdxColorAqua = 'Aqua';
  sdxColorTurquoise = 'Turquoise';
  sdxColorLightTurquoise = 'Light Turquoise';
  sdxColorDarkBlue = 'Dark Blue';
  sdxColorBlue = 'Blue';
  sdxColorLightBlue = 'Light Blue';
  sdxColorSkyBlue = 'Sky Blue';
  sdxColorPaleBlue = 'Pale Blue';
  sdxColorIndigo = 'Indigo';
  sdxColorBlueGray = 'Blue Gray';
  sdxColorViolet = 'Violet';
  sdxColorPlum = 'Plum';
  sdxColorLavender = 'Lavender';
  sdxColorGray80 = 'Gray-80%';
  sdxColorGray50 = 'Gray-50%';
  sdxColorGray40 = 'Gray-40%';
  sdxColorGray25 = 'Gray-25%';
  sdxColorWhite = 'White';

implementation

uses
  dxCoreClasses, dxCore;

procedure AddExpressPrintingSystemResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxColorBlack', @sdxColorBlack);
  AProduct.Add('sdxColorDarkRed', @sdxColorDarkRed);
  AProduct.Add('sdxColorRed', @sdxColorRed);
  AProduct.Add('sdxColorPink', @sdxColorPink);
  AProduct.Add('sdxColorRose', @sdxColorRose);
  AProduct.Add('sdxColorBrown', @sdxColorBrown);
  AProduct.Add('sdxColorOrange', @sdxColorOrange);
  AProduct.Add('sdxColorLightOrange', @sdxColorLightOrange);
  AProduct.Add('sdxColorGold', @sdxColorGold);
  AProduct.Add('sdxColorTan', @sdxColorTan);
  AProduct.Add('sdxColorOliveGreen', @sdxColorOliveGreen);
  AProduct.Add('sdxColorDrakYellow', @sdxColorDrakYellow);
  AProduct.Add('sdxColorLime', @sdxColorLime);
  AProduct.Add('sdxColorYellow', @sdxColorYellow);
  AProduct.Add('sdxColorLightYellow', @sdxColorLightYellow);
  AProduct.Add('sdxColorDarkGreen', @sdxColorDarkGreen);
  AProduct.Add('sdxColorGreen', @sdxColorGreen);
  AProduct.Add('sdxColorSeaGreen', @sdxColorSeaGreen);
  AProduct.Add('sdxColorBrighthGreen', @sdxColorBrighthGreen);
  AProduct.Add('sdxColorLightGreen', @sdxColorLightGreen);
  AProduct.Add('sdxColorDarkTeal', @sdxColorDarkTeal);
  AProduct.Add('sdxColorTeal', @sdxColorTeal);
  AProduct.Add('sdxColorAqua', @sdxColorAqua);
  AProduct.Add('sdxColorTurquoise', @sdxColorTurquoise);
  AProduct.Add('sdxColorLightTurquoise', @sdxColorLightTurquoise);
  AProduct.Add('sdxColorDarkBlue', @sdxColorDarkBlue);
  AProduct.Add('sdxColorBlue', @sdxColorBlue);
  AProduct.Add('sdxColorLightBlue', @sdxColorLightBlue);
  AProduct.Add('sdxColorSkyBlue', @sdxColorSkyBlue);
  AProduct.Add('sdxColorPaleBlue', @sdxColorPaleBlue);
  AProduct.Add('sdxColorIndigo', @sdxColorIndigo);
  AProduct.Add('sdxColorBlueGray', @sdxColorBlueGray);
  AProduct.Add('sdxColorViolet', @sdxColorViolet);
  AProduct.Add('sdxColorPlum', @sdxColorPlum);
  AProduct.Add('sdxColorLavender', @sdxColorLavender);
  AProduct.Add('sdxColorGray80', @sdxColorGray80);
  AProduct.Add('sdxColorGray50', @sdxColorGray50);
  AProduct.Add('sdxColorGray40', @sdxColorGray40);
  AProduct.Add('sdxColorGray25', @sdxColorGray25);
  AProduct.Add('sdxColorWhite', @sdxColorWhite);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressPrinting System', @AddExpressPrintingSystemResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressPrinting System');
end.
