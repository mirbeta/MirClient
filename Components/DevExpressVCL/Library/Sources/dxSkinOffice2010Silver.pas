{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSkins Library                                     }
{                                                                    }
{           Copyright (c) 2006-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSKINS AND ALL ACCOMPANYING     }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxSkinOffice2010Silver;

{$I cxVer.inc}

interface

uses
  Classes, dxCore, dxCoreGraphics, dxGDIPlusApi, cxLookAndFeelPainters, dxSkinsCore, dxSkinsLookAndFeelPainter;

{$IFDEF WIN64}

{$IF DEFINED(VER210)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS14.a"'}
{$ELSEIF DEFINED(VER220)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS15.a"'}
{$ELSEIF DEFINED(VER230)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS16.a"'}
{$ELSEIF DEFINED(VER240)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS17.a"'}
{$ELSEIF DEFINED(VER250)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS18.a"'}
{$ELSEIF DEFINED(VER260)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS19.a"'}
{$ELSEIF DEFINED(VER270)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS20.a"'}
{$ELSEIF DEFINED(VER280)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS21.a"'}
{$ELSEIF DEFINED(VER290)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS22.a"'}
{$ELSEIF DEFINED(VER300)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS23.a"'}
{$ELSEIF DEFINED(VER310)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS24.a"'}
{$ELSEIF DEFINED(VER320)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS25.a"'}
{$ELSEIF DEFINED(VER330)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS26.a"'}
{$ELSEIF DEFINED(VER340)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS27.a"'}
{$ELSE}
  Unsupported
{$IFEND}

{$ELSE}

{$IF DEFINED(VER210)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS14.lib"'}
{$ELSEIF DEFINED(VER220)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS15.lib"'}
{$ELSEIF DEFINED(VER230)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS16.lib"'}
{$ELSEIF DEFINED(VER240)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS17.lib"'}
{$ELSEIF DEFINED(VER250)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS18.lib"'}
{$ELSEIF DEFINED(VER260)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS19.lib"'}
{$ELSEIF DEFINED(VER270)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS20.lib"'}
{$ELSEIF DEFINED(VER280)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS21.lib"'}
{$ELSEIF DEFINED(VER290)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS22.lib"'}
{$ELSEIF DEFINED(VER300)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS23.lib"'}
{$ELSEIF DEFINED(VER310)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS24.lib"'}
{$ELSEIF DEFINED(VER320)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS25.lib"'}
{$ELSEIF DEFINED(VER330)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS26.lib"'}
{$ELSEIF DEFINED(VER340)}
  {$HPPEMIT '#pragma link "dxSkinOffice2010SilverRS27.lib"'}

{$ELSE}
  Unsupported
{$IFEND}

{$ENDIF}
type

  { TdxSkinOffice2010SilverPainter }

  TdxSkinOffice2010SilverPainter = class(TdxSkinLookAndFeelPainter)
  public
    function LookAndFeelName: string; override;
  end;

implementation

{$R dxSkinOffice2010Silver.res}

const
  SkinsCount = 1;
  SkinNames: array[0..SkinsCount - 1] of string = (
    'Office2010Silver'
  );
  SkinPainters: array[0..SkinsCount - 1] of TdxSkinLookAndFeelPainterClass = (
    TdxSkinOffice2010SilverPainter
  );


{ TdxSkinOffice2010SilverPainter }

function TdxSkinOffice2010SilverPainter.LookAndFeelName: string;
begin
  Result := 'Office2010Silver';
end;

//

procedure RegisterPainters;
var
  I: Integer;
begin
  if CheckGdiPlus then
  begin
    for I := 0 to SkinsCount - 1 do
      cxLookAndFeelPaintersManager.Register(SkinPainters[I].Create(SkinNames[I], HInstance));
  end;
end;

procedure UnregisterPainters;
var
  I: Integer;
begin
  if cxLookAndFeelPaintersManager <> nil then
  begin
    for I := 0 to SkinsCount - 1 do
      cxLookAndFeelPaintersManager.Unregister(SkinNames[I]);
  end;
end;

{$IFNDEF DXSKINDYNAMICLOADING}
initialization
  dxUnitsLoader.AddUnit(@RegisterPainters, @UnregisterPainters);
finalization
  dxUnitsLoader.RemoveUnit(@UnregisterPainters);
{$ENDIF}
end.