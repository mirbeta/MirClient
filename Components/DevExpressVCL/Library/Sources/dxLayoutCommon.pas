{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl common routines                     }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
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

unit dxLayoutCommon;

{$I cxVer.inc}

interface

uses
  SysUtils, TypInfo, Windows, Classes, Graphics, Forms, cxClasses;

type
  TdxLayoutSide = (sdLeft, sdRight, sdTop, sdBottom);
  TdxAlignmentVert = (tavTop, tavCenter, tavBottom);

  IdxLayoutComponent = interface
  ['{F31C9078-5732-44D8-9347-3EA7B93837E3}']
    procedure SelectionChanged; stdcall;
  end;

  { IdxLayoutControlDesignerHelper }

  IdxLayoutDesignTimeHelper = interface
  ['{9A1C2CD3-7CD9-4A7D-8E51-9305994B2F2E}']
    function IsToolSelected: Boolean;
    function GetFieldDisplayName(AField: TObject): string;
  end;

function GetHotTrackColor: TColor;
function GetPlainString(const S: string): string;
procedure SetComponentName(AComponent: TComponent; const ABaseName: string; AIsDesigning, AIsLoading: Boolean);

var
  dxLayoutDesignTimeHelper: IdxLayoutDesignTimeHelper;

implementation

uses
  dxCore, dxLayoutStrs;

{ routines }

function GetHotTrackColor: TColor;
begin
  Result := GetSysColor(COLOR_HOTLIGHT);
end;

function GetPlainString(const S: string): string;
const
  SpecialChars = [#10, #13];
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if dxCharInSet(Result[I], SpecialChars) then
      Delete(Result, I, 1);
end;

procedure SetComponentName(AComponent: TComponent; const ABaseName: string;
  AIsDesigning, AIsLoading: Boolean);
begin
  AComponent.Name := GetValidName(AComponent, ABaseName, True);
end;

end.
