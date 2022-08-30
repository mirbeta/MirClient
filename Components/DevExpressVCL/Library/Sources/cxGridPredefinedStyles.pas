{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxGridPredefinedStyles;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Forms, cxClasses, cxStyles,
  cxGridTableView, cxGridBandedTableView, cxGridCardView;

type
  TcxdmGridPredefinedStyles = class(TDataModule)
    StyleRepository: TcxStyleRepository;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    cxStyle8: TcxStyle;
    cxStyle9: TcxStyle;
    cxStyle10: TcxStyle;
    cxStyle11: TcxStyle;
    cxStyle12: TcxStyle;
    cxStyle13: TcxStyle;
    cxStyle14: TcxStyle;
    cxStyle15: TcxStyle;
    cxStyle16: TcxStyle;
    cxStyle17: TcxStyle;
    cxStyle18: TcxStyle;
    cxStyle19: TcxStyle;
    cxStyle20: TcxStyle;
    cxStyle21: TcxStyle;
    cxStyle22: TcxStyle;
    cxStyle23: TcxStyle;
    cxStyle24: TcxStyle;
    cxStyle25: TcxStyle;
    cxStyle26: TcxStyle;
    cxStyle27: TcxStyle;
    cxStyle28: TcxStyle;
    cxStyle29: TcxStyle;
    cxStyle30: TcxStyle;
    cxStyle31: TcxStyle;
    cxStyle32: TcxStyle;
    cxStyle33: TcxStyle;
    cxStyle34: TcxStyle;
    cxStyle35: TcxStyle;
    cxStyle36: TcxStyle;
    cxStyle37: TcxStyle;
    cxStyle38: TcxStyle;
    cxStyle39: TcxStyle;
    cxStyle40: TcxStyle;
    cxStyle41: TcxStyle;
    cxStyle42: TcxStyle;
    cxStyle43: TcxStyle;
    cxStyle44: TcxStyle;
    cxStyle45: TcxStyle;
    cxStyle46: TcxStyle;
    cxStyle47: TcxStyle;
    cxStyle48: TcxStyle;
    cxStyle49: TcxStyle;
    cxStyle50: TcxStyle;
    cxStyle51: TcxStyle;
    cxStyle52: TcxStyle;
    cxStyle53: TcxStyle;
    cxStyle54: TcxStyle;
    cxStyle55: TcxStyle;
    cxStyle56: TcxStyle;
    cxStyle57: TcxStyle;
    cxStyle58: TcxStyle;
    cxStyle59: TcxStyle;
    cxStyle60: TcxStyle;
    cxStyle61: TcxStyle;
    cxStyle62: TcxStyle;
    cxStyle63: TcxStyle;
    cxStyle64: TcxStyle;
    cxStyle65: TcxStyle;
    cxStyle66: TcxStyle;
    cxStyle67: TcxStyle;
    cxStyle68: TcxStyle;
    cxStyle69: TcxStyle;
    cxStyle70: TcxStyle;
    cxStyle71: TcxStyle;
    cxStyle72: TcxStyle;
    cxStyle73: TcxStyle;
    cxStyle74: TcxStyle;
    cxStyle75: TcxStyle;
    cxStyle76: TcxStyle;
    cxStyle77: TcxStyle;
    cxStyle78: TcxStyle;
    cxStyle79: TcxStyle;
    cxStyle80: TcxStyle;
    cxStyle81: TcxStyle;
    cxStyle82: TcxStyle;
    cxStyle83: TcxStyle;
    cxStyle84: TcxStyle;
    cxStyle85: TcxStyle;
    cxStyle86: TcxStyle;
    cxStyle87: TcxStyle;
    cxStyle88: TcxStyle;
    cxStyle89: TcxStyle;
    cxStyle90: TcxStyle;
    cxStyle91: TcxStyle;
    cxStyle92: TcxStyle;
    cxStyle93: TcxStyle;
    cxStyle94: TcxStyle;
    cxStyle95: TcxStyle;
    cxStyle96: TcxStyle;
    cxStyle97: TcxStyle;
    cxStyle98: TcxStyle;
    cxStyle99: TcxStyle;
    cxStyle100: TcxStyle;
    cxStyle101: TcxStyle;
    cxStyle102: TcxStyle;
    cxStyle103: TcxStyle;
    cxStyle104: TcxStyle;
    cxStyle105: TcxStyle;
    cxStyle106: TcxStyle;
    cxStyle107: TcxStyle;
    cxStyle108: TcxStyle;
    cxStyle109: TcxStyle;
    cxStyle110: TcxStyle;
    cxStyle111: TcxStyle;
    cxStyle112: TcxStyle;
    cxStyle113: TcxStyle;
    cxStyle114: TcxStyle;
    cxStyle115: TcxStyle;
    cxStyle116: TcxStyle;
    cxStyle117: TcxStyle;
    cxStyle118: TcxStyle;
    cxStyle119: TcxStyle;
    cxStyle120: TcxStyle;
    cxStyle121: TcxStyle;
    cxStyle122: TcxStyle;
    cxStyle123: TcxStyle;
    cxStyle124: TcxStyle;
    cxStyle125: TcxStyle;
    cxStyle126: TcxStyle;
    cxStyle127: TcxStyle;
    cxStyle128: TcxStyle;
    cxStyle129: TcxStyle;
    cxStyle130: TcxStyle;
    cxStyle131: TcxStyle;
    cxStyle132: TcxStyle;
    cxStyle133: TcxStyle;
    cxStyle134: TcxStyle;
    cxStyle135: TcxStyle;
    cxStyle136: TcxStyle;
    cxStyle137: TcxStyle;
    cxStyle138: TcxStyle;
    cxStyle139: TcxStyle;
    cxStyle140: TcxStyle;
    cxStyle141: TcxStyle;
    cxStyle142: TcxStyle;
    cxStyle143: TcxStyle;
    cxStyle144: TcxStyle;
    cxStyle145: TcxStyle;
    cxStyle146: TcxStyle;
    cxStyle147: TcxStyle;
    cxStyle148: TcxStyle;
    cxStyle149: TcxStyle;
    cxStyle150: TcxStyle;
    cxStyle151: TcxStyle;
    cxStyle152: TcxStyle;
    cxStyle153: TcxStyle;
    cxStyle154: TcxStyle;
    cxStyle155: TcxStyle;
    cxStyle156: TcxStyle;
    cxStyle157: TcxStyle;
    cxStyle158: TcxStyle;
    cxStyle159: TcxStyle;
    cxStyle160: TcxStyle;
    cxStyle161: TcxStyle;
    cxStyle162: TcxStyle;
    cxStyle163: TcxStyle;
    cxStyle164: TcxStyle;
    cxStyle165: TcxStyle;
    cxStyle166: TcxStyle;
    cxStyle167: TcxStyle;
    cxStyle168: TcxStyle;
    cxStyle169: TcxStyle;
    cxStyle170: TcxStyle;
    cxStyle171: TcxStyle;
    cxStyle172: TcxStyle;
    cxStyle173: TcxStyle;
    cxStyle174: TcxStyle;
    cxStyle175: TcxStyle;
    cxStyle176: TcxStyle;
    cxStyle177: TcxStyle;
    cxStyle178: TcxStyle;
    cxStyle179: TcxStyle;
    cxStyle180: TcxStyle;
    cxStyle181: TcxStyle;
    cxStyle182: TcxStyle;
    cxStyle183: TcxStyle;
    cxStyle184: TcxStyle;
    cxStyle185: TcxStyle;
    cxStyle186: TcxStyle;
    cxStyle187: TcxStyle;
    cxStyle188: TcxStyle;
    cxStyle189: TcxStyle;
    cxStyle190: TcxStyle;
    cxStyle191: TcxStyle;
    cxStyle192: TcxStyle;
    cxStyle193: TcxStyle;
    cxStyle194: TcxStyle;
    cxStyle195: TcxStyle;
    cxStyle196: TcxStyle;
    cxStyle197: TcxStyle;
    cxStyle198: TcxStyle;
    cxStyle199: TcxStyle;
    cxStyle200: TcxStyle;
    cxStyle201: TcxStyle;
    cxStyle202: TcxStyle;
    cxStyle203: TcxStyle;
    cxStyle204: TcxStyle;
    cxStyle205: TcxStyle;
    cxStyle206: TcxStyle;
    cxStyle207: TcxStyle;
    cxStyle208: TcxStyle;
    cxStyle209: TcxStyle;
    cxStyle210: TcxStyle;
    cxStyle211: TcxStyle;
    cxStyle212: TcxStyle;
    cxStyle213: TcxStyle;
    cxStyle214: TcxStyle;
    cxStyle215: TcxStyle;
    cxStyle216: TcxStyle;
    cxStyle217: TcxStyle;
    cxStyle218: TcxStyle;
    cxStyle219: TcxStyle;
    cxStyle220: TcxStyle;
    cxStyle221: TcxStyle;
    cxStyle222: TcxStyle;
    cxStyle223: TcxStyle;
    cxStyle224: TcxStyle;
    cxStyle225: TcxStyle;
    cxStyle226: TcxStyle;
    cxStyle227: TcxStyle;
    cxStyle228: TcxStyle;
    cxStyle229: TcxStyle;
    cxStyle230: TcxStyle;
    cxStyle231: TcxStyle;
    cxStyle232: TcxStyle;
    cxStyle233: TcxStyle;
    cxStyle234: TcxStyle;
    cxStyle235: TcxStyle;
    cxStyle236: TcxStyle;
    cxStyle237: TcxStyle;
    cxStyle238: TcxStyle;
    cxStyle239: TcxStyle;
    cxStyle240: TcxStyle;
    cxStyle241: TcxStyle;
    cxStyle242: TcxStyle;
    cxStyle243: TcxStyle;
    cxStyle244: TcxStyle;
    cxStyle245: TcxStyle;
    cxStyle246: TcxStyle;
    cxStyle247: TcxStyle;
    cxStyle248: TcxStyle;
    cxStyle249: TcxStyle;
    cxStyle250: TcxStyle;
    cxStyle251: TcxStyle;
    cxStyle252: TcxStyle;
    cxStyle253: TcxStyle;
    cxStyle254: TcxStyle;
    cxStyle255: TcxStyle;
    cxStyle256: TcxStyle;
    cxStyle257: TcxStyle;
    cxStyle258: TcxStyle;
    cxStyle259: TcxStyle;
    cxStyle260: TcxStyle;
    cxStyle261: TcxStyle;
    cxStyle262: TcxStyle;
    cxStyle263: TcxStyle;
    cxStyle264: TcxStyle;
    cxStyle265: TcxStyle;
    cxStyle266: TcxStyle;
    cxStyle267: TcxStyle;
    cxStyle268: TcxStyle;
    cxStyle269: TcxStyle;
    cxStyle270: TcxStyle;
    cxStyle271: TcxStyle;
    cxStyle272: TcxStyle;
    cxStyle273: TcxStyle;
    cxStyle274: TcxStyle;
    cxStyle275: TcxStyle;
    cxStyle276: TcxStyle;
    cxStyle277: TcxStyle;
    cxStyle278: TcxStyle;
    cxStyle279: TcxStyle;
    cxStyle280: TcxStyle;
    cxStyle281: TcxStyle;
    cxStyle282: TcxStyle;
    cxStyle283: TcxStyle;
    cxStyle284: TcxStyle;
    cxStyle285: TcxStyle;
    cxStyle286: TcxStyle;
    cxStyle287: TcxStyle;
    cxStyle288: TcxStyle;
    cxStyle289: TcxStyle;
    cxStyle290: TcxStyle;
    cxStyle291: TcxStyle;
    cxStyle292: TcxStyle;
    cxStyle293: TcxStyle;
    cxStyle294: TcxStyle;
    cxStyle295: TcxStyle;
    cxStyle296: TcxStyle;
    cxStyle297: TcxStyle;
    cxStyle298: TcxStyle;
    cxStyle299: TcxStyle;
    cxStyle300: TcxStyle;
    cxStyle301: TcxStyle;
    cxStyle302: TcxStyle;
    cxStyle303: TcxStyle;
    cxStyle304: TcxStyle;
    cxStyle305: TcxStyle;
    cxStyle306: TcxStyle;
    cxStyle307: TcxStyle;
    cxStyle308: TcxStyle;
    cxStyle309: TcxStyle;
    cxStyle310: TcxStyle;
    cxStyle311: TcxStyle;
    cxStyle312: TcxStyle;
    cxStyle313: TcxStyle;
    cxStyle314: TcxStyle;
    cxStyle315: TcxStyle;
    cxStyle316: TcxStyle;
    cxStyle317: TcxStyle;
    cxStyle318: TcxStyle;
    cxStyle319: TcxStyle;
    cxStyle320: TcxStyle;
    cxStyle321: TcxStyle;
    cxStyle322: TcxStyle;
    cxStyle323: TcxStyle;
    cxStyle324: TcxStyle;
    cxStyle325: TcxStyle;
    cxStyle326: TcxStyle;
    cxStyle327: TcxStyle;
    cxStyle328: TcxStyle;
    cxStyle329: TcxStyle;
    cxStyle330: TcxStyle;
    cxStyle331: TcxStyle;
    cxStyle332: TcxStyle;
    cxStyle333: TcxStyle;
    cxStyle334: TcxStyle;
    cxStyle335: TcxStyle;
    cxStyle336: TcxStyle;
    cxStyle337: TcxStyle;
    cxStyle338: TcxStyle;
    cxStyle339: TcxStyle;
    cxStyle340: TcxStyle;
    cxStyle341: TcxStyle;
    cxStyle342: TcxStyle;
    cxStyle343: TcxStyle;
    cxStyle344: TcxStyle;
    cxStyle345: TcxStyle;
    cxStyle346: TcxStyle;
    cxStyle347: TcxStyle;
    cxStyle348: TcxStyle;
    cxStyle349: TcxStyle;
    cxStyle350: TcxStyle;
    cxStyle351: TcxStyle;
    cxStyle352: TcxStyle;
    cxStyle353: TcxStyle;
    cxStyle354: TcxStyle;
    cxStyle355: TcxStyle;
    cxStyle356: TcxStyle;
    cxStyle357: TcxStyle;
    cxStyle358: TcxStyle;
    cxStyle359: TcxStyle;
    cxStyle360: TcxStyle;
    cxStyle361: TcxStyle;
    cxStyle362: TcxStyle;
    cxStyle363: TcxStyle;
    GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetDevExpress: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetUserFormat1: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetUserFormat1: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetUserFormat1: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetUserFormat2: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetUserFormat2: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetUserFormat2: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetUserFormat3: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetUserFormat3: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetUserFormat3: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetUserFormat4: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetUserFormat4: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetUserFormat4: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetBrick: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetBrick: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetBrick: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetDesert: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetDesert: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetDesert: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetEggplant: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetEggplant: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetEggplant: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetLilac: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetLilac: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetLilac: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetMaple: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetMaple: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetMaple: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetMarinehighcolor: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetMarinehighcolor: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetMarinehighcolor: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetPlumhighcolor: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetPlumhighcolor: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetPlumhighcolor: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetPumpkinlarge: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetPumpkinlarge: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetPumpkinlarge: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetRainyDay: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetRainyDay: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetRainyDay: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetRedWhiteandBlueVGA: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetRedWhiteandBlueVGA: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetRedWhiteandBlueVGA: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetRose: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetRose: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetRose: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetRoselarge: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetRoselarge: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetRoselarge: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetSlate: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetSlate: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetSlate: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetSpruce: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetSpruce: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetSpruce: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetStormVGA: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetStormVGA: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetStormVGA: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetTealVGA: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetTealVGA: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetTealVGA: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetWheat: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetWheat: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetWheat: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetWindowsClassic: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetWindowsClassic: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetWindowsClassic: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetWindowsClassiclarge: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetWindowsClassiclarge: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetWindowsClassiclarge: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetWindowsStandard: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetWindowsStandard: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetWindowsStandard: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetWindowsStandardlarge: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetWindowsStandardlarge: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetWindowsStandardlarge: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetHighContrast1: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetHighContrast1: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetHighContrast1: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetHighContrast1large: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetHighContrast1large: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetHighContrast1large: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetHighContrast2: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetHighContrast2: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetHighContrast2: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetHighContrast2large: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetHighContrast2large: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetHighContrast2large: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetHighContrastBlack: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetHighContrastBlack: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetHighContrastBlack: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetHighContrastBlacklarge: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetHighContrastBlacklarge: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetHighContrastBlacklarge: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetHighContrastWhite: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetHighContrastWhite: TcxGridBandedTableViewStyleSheet;
    GridCardViewStyleSheetHighContrastWhite: TcxGridCardViewStyleSheet;
    GridTableViewStyleSheetHighContrastWhitelarge: TcxGridTableViewStyleSheet;
    GridBandedTableViewStyleSheetHighContrastWhitelarge: TcxGridBandedTableViewStyleSheet;
    ClassicRowCaption: TcxStyle;
    cxStyle364: TcxStyle;
    cxStyle365: TcxStyle;
    cxStyle366: TcxStyle;
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
TcxGridPredefinedStyleSheets = class(TcxPredefinedStyleSheets)
private
  FData: TcxdmGridPredefinedStyles;
protected
  procedure AddStyleSheets; override;
public
  constructor Create; override;
  destructor Destroy; override;
end;

{ TcxGridPredefinedStyleSheets }
constructor TcxGridPredefinedStyleSheets.Create;
begin
  inherited Create;
  FData := TcxdmGridPredefinedStyles.Create(nil);
  AddStyleSheets;
end;

destructor TcxGridPredefinedStyleSheets.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TcxGridPredefinedStyleSheets.AddStyleSheets;
var
  I: Integer;
begin
  for I := 0 to FData.StyleRepository.StyleSheetCount - 1 do
    AddStyleSheet(FData.StyleRepository.StyleSheets[I]);
end;

initialization
  RegisterPredefinedStyleSheets(TcxGridPredefinedStyleSheets);

finalization
  UnregisterPredefinedStyleSheets(TcxGridPredefinedStyleSheets);

end.
