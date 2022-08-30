{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPivotGrid                                         }
{                                                                    }
{           Copyright (c) 2005-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPIVOTGRID AND ALL ACCOMPANYING }
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

unit cxPivotGridPredefinedStyles;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Forms, cxClasses, cxStyles, cxCustomPivotGrid;

type
  TcxdmPivotGridPredefinedStyles = class(TDataModule)
    StyleRepository: TcxStyleRepository;
    ClassicBackground: TcxStyle;
    ClassicContent: TcxStyle;
    ClassicHeader: TcxStyle;
    ClassicInactive: TcxStyle;
    ClassicSelection: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle10: TcxStyle;
    cxStyle13: TcxStyle;
    cxStyle14: TcxStyle;
    cxStyle17: TcxStyle;
    cxStyle21: TcxStyle;
    cxStyle24: TcxStyle;
    cxStyle25: TcxStyle;
    cxStyle28: TcxStyle;
    cxStyle32: TcxStyle;
    cxStyle35: TcxStyle;
    cxStyle36: TcxStyle;
    cxStyle39: TcxStyle;
    cxStyle43: TcxStyle;
    cxStyle46: TcxStyle;
    cxStyle47: TcxStyle;
    cxStyle50: TcxStyle;
    cxStyle54: TcxStyle;
    cxStyle57: TcxStyle;
    cxStyle58: TcxStyle;
    cxStyle61: TcxStyle;
    cxStyle65: TcxStyle;
    cxStyle68: TcxStyle;
    cxStyle69: TcxStyle;
    cxStyle72: TcxStyle;
    cxStyle76: TcxStyle;
    cxStyle79: TcxStyle;
    cxStyle80: TcxStyle;
    cxStyle83: TcxStyle;
    cxStyle87: TcxStyle;
    cxStyle90: TcxStyle;
    cxStyle91: TcxStyle;
    cxStyle94: TcxStyle;
    cxStyle98: TcxStyle;
    cxStyle101: TcxStyle;
    cxStyle102: TcxStyle;
    cxStyle105: TcxStyle;
    cxStyle109: TcxStyle;
    cxStyle112: TcxStyle;
    cxStyle113: TcxStyle;
    cxStyle116: TcxStyle;
    cxStyle120: TcxStyle;
    cxStyle123: TcxStyle;
    cxStyle124: TcxStyle;
    cxStyle127: TcxStyle;
    cxStyle131: TcxStyle;
    cxStyle134: TcxStyle;
    cxStyle135: TcxStyle;
    cxStyle138: TcxStyle;
    cxStyle142: TcxStyle;
    cxStyle145: TcxStyle;
    cxStyle146: TcxStyle;
    cxStyle149: TcxStyle;
    cxStyle153: TcxStyle;
    cxStyle156: TcxStyle;
    cxStyle157: TcxStyle;
    cxStyle160: TcxStyle;
    cxStyle164: TcxStyle;
    cxStyle167: TcxStyle;
    cxStyle168: TcxStyle;
    cxStyle171: TcxStyle;
    cxStyle175: TcxStyle;
    cxStyle178: TcxStyle;
    cxStyle179: TcxStyle;
    cxStyle182: TcxStyle;
    cxStyle186: TcxStyle;
    cxStyle189: TcxStyle;
    cxStyle190: TcxStyle;
    cxStyle193: TcxStyle;
    cxStyle197: TcxStyle;
    cxStyle200: TcxStyle;
    cxStyle201: TcxStyle;
    cxStyle204: TcxStyle;
    cxStyle208: TcxStyle;
    cxStyle211: TcxStyle;
    cxStyle212: TcxStyle;
    cxStyle215: TcxStyle;
    cxStyle219: TcxStyle;
    cxStyle222: TcxStyle;
    cxStyle223: TcxStyle;
    cxStyle226: TcxStyle;
    cxStyle230: TcxStyle;
    cxStyle233: TcxStyle;
    cxStyle234: TcxStyle;
    cxStyle237: TcxStyle;
    cxStyle241: TcxStyle;
    cxStyle244: TcxStyle;
    cxStyle245: TcxStyle;
    cxStyle248: TcxStyle;
    cxStyle252: TcxStyle;
    cxStyle255: TcxStyle;
    cxStyle256: TcxStyle;
    cxStyle259: TcxStyle;
    cxStyle263: TcxStyle;
    cxStyle266: TcxStyle;
    cxStyle267: TcxStyle;
    cxStyle270: TcxStyle;
    cxStyle274: TcxStyle;
    cxStyle277: TcxStyle;
    cxStyle278: TcxStyle;
    cxStyle281: TcxStyle;
    cxStyle285: TcxStyle;
    cxStyle286: TcxStyle;
    cxStyle288: TcxStyle;
    cxStyle289: TcxStyle;
    cxStyle292: TcxStyle;
    cxStyle296: TcxStyle;
    cxStyle297: TcxStyle;
    cxStyle299: TcxStyle;
    cxStyle300: TcxStyle;
    cxStyle303: TcxStyle;
    cxStyle307: TcxStyle;
    cxStyle308: TcxStyle;
    cxStyle310: TcxStyle;
    cxStyle311: TcxStyle;
    cxStyle314: TcxStyle;
    cxStyle318: TcxStyle;
    cxStyle319: TcxStyle;
    cxStyle321: TcxStyle;
    cxStyle322: TcxStyle;
    cxStyle325: TcxStyle;
    cxStyle329: TcxStyle;
    cxStyle330: TcxStyle;
    cxStyle332: TcxStyle;
    cxStyle333: TcxStyle;
    cxStyle336: TcxStyle;
    cxStyle340: TcxStyle;
    cxStyle341: TcxStyle;
    cxStyle343: TcxStyle;
    cxStyle344: TcxStyle;
    cxStyle347: TcxStyle;
    cxStyle351: TcxStyle;
    cxStyle354: TcxStyle;
    cxStyle355: TcxStyle;
    cxStyle358: TcxStyle;
    cxStyle362: TcxStyle;
    PivotGridStyleSheetDevExpress: TcxPivotGridStyleSheet;
    PivotGridStyleSheetUserFormat1: TcxPivotGridStyleSheet;
    PivotGridStyleSheetUserFormat2: TcxPivotGridStyleSheet;
    PivotGridStyleSheetUserFormat3: TcxPivotGridStyleSheet;
    PivotGridStyleSheetUserFormat4: TcxPivotGridStyleSheet;
    PivotGridStyleSheetBrick: TcxPivotGridStyleSheet;
    PivotGridStyleSheetDesert: TcxPivotGridStyleSheet;
    PivotGridStyleSheetEggplant: TcxPivotGridStyleSheet;
    PivotGridStyleSheetLilac: TcxPivotGridStyleSheet;
    PivotGridStyleSheetMaple: TcxPivotGridStyleSheet;
    PivotGridStyleSheetMarinehighcolor: TcxPivotGridStyleSheet;
    PivotGridStyleSheetPlumhighcolor: TcxPivotGridStyleSheet;
    PivotGridStyleSheetPumpkinlarge: TcxPivotGridStyleSheet;
    PivotGridStyleSheetRainyDay: TcxPivotGridStyleSheet;
    PivotGridStyleSheetRedWhiteandBlueVGA: TcxPivotGridStyleSheet;
    PivotGridStyleSheetRose: TcxPivotGridStyleSheet;
    PivotGridStyleSheetRoselarge: TcxPivotGridStyleSheet;
    PivotGridStyleSheetSlate: TcxPivotGridStyleSheet;
    PivotGridStyleSheetSpruce: TcxPivotGridStyleSheet;
    PivotGridStyleSheetStormVGA: TcxPivotGridStyleSheet;
    PivotGridStyleSheetTealVGA: TcxPivotGridStyleSheet;
    PivotGridStyleSheetWheat: TcxPivotGridStyleSheet;
    PivotGridStyleSheetWindowsClassic: TcxPivotGridStyleSheet;
    PivotGridStyleSheetWindowsClassiclarge: TcxPivotGridStyleSheet;
    PivotGridStyleSheetWindowsStandard: TcxPivotGridStyleSheet;
    PivotGridStyleSheetWindowsStandardlarge: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrast1: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrast1large: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrast2: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrast2large: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrastBlack: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrastBlacklarge: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrastWhite: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrastWhitelarge: TcxPivotGridStyleSheet;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  cxStyleSheetsLoad;

type
  TcxPivotGridPredefinedStyleSheets = class(TcxPredefinedStyleSheets)
  private
    FData: TcxdmPivotGridPredefinedStyles;
  protected
    procedure AddStyleSheets; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{ TcxPivotGridPredefinedStyleSheets }

constructor TcxPivotGridPredefinedStyleSheets.Create;
begin
  inherited Create;
  FData := TcxdmPivotGridPredefinedStyles.Create(nil);
  AddStyleSheets;
end;

destructor TcxPivotGridPredefinedStyleSheets.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TcxPivotGridPredefinedStyleSheets.AddStyleSheets;
var
  I: Integer;
begin
  for I := 0 to FData.StyleRepository.StyleSheetCount - 1 do
    AddStyleSheet(FData.StyleRepository.StyleSheets[I]);
end;

initialization
  RegisterPredefinedStyleSheets(TcxPivotGridPredefinedStyleSheets);

finalization
  UnregisterPredefinedStyleSheets(TcxPivotGridPredefinedStyleSheets);

end.
