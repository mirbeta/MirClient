{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxMapControlStrs;

{$I cxVer.Inc}

interface

uses
  dxCore, cxClasses;

resourcestring
  sdxMapControlWest   = 'W';
  sdxMapControlEast  = 'E';
  sdxMapControlSouth   = 'S';
  sdxMapControlNorth  = 'N';
  sdxMapControlKilometers = 'km';
  sdxMapControlMeters = 'm';
  sdxMapControlMiles = 'mi';
  sdxMapControlFeet = 'ft';

implementation

procedure AddMapControlResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxMapControlWest', @sdxMapControlWest);
  AProduct.Add('sdxMapControlEast', @sdxMapControlEast);
  AProduct.Add('sdxMapControlSouth', @sdxMapControlSouth);
  AProduct.Add('sdxMapControlNorth', @sdxMapControlNorth);
  AProduct.Add('sdxMapControlKilometers', @sdxMapControlKilometers);
  AProduct.Add('sdxMapControlMeters', @sdxMapControlMeters);
  AProduct.Add('sdxMapControlMiles', @sdxMapControlMiles);
  AProduct.Add('sdxMapControlFeet', @sdxMapControlFeet);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressMapControl', @AddMapControlResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressMapControl', @AddMapControlResourceStringNames);
end.
