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
{   EXECUTABLE PROGRAM ONLY                                          }
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

unit dxPScxGridLnkPredefinedStyleSheets;

interface

{$I cxVer.inc}

uses
  Classes, cxClasses, cxStyles, dxPScxGridLnk, dxPScxCommon;

type
  TdxdmPScxGridLnkPredefinedStyles = class(TDataModule)
    StyleRepository: TcxStyleRepository;
    styleGreenFixed: TcxStyle;
    styleGreenLightContent: TcxStyle;
    styleProfessionalFixed: TcxStyle;
    styleGreenLightPreview: TcxStyle;
    ssGreen: TdxGridReportLinkStyleSheet;
    ssProfessional: TdxGridReportLinkStyleSheet;
    styleGreenFooter: TcxStyle;
    styleNoneContent: TcxStyle;
    styleNoneFixed: TcxStyle;
    styleProfessionalGroup: TcxStyle;
    styleProfessionalContentOdd: TcxStyle;
    styleProfessionalContentEven: TcxStyle;
    styleProfessionalSelection: TcxStyle;
    styleProfessionalPreview: TcxStyle;
    styleGreenSelected: TcxStyle;
    styleGreenGroup: TcxStyle;
    styleProfessionalRowCaption: TcxStyle;
    styleProfessionalCaptionRow: TcxStyle;
    ssTransparent: TdxGridReportLinkStyleSheet;
    styleTransparentContent: TcxStyle;
    styleTransparentBandHeader: TcxStyle;
    styleTransparentHeader: TcxStyle;
    styleTransparentFooter: TcxStyle;
    styleTransparentGroup: TcxStyle;
    styleTransparentPreview: TcxStyle;
    styleTransparentLevelCaption: TcxStyle;
    styleTransparentFilterBar: TcxStyle;
    styleTransparentCaptionRow: TcxStyle;
    styleTransparentRowCaption: TcxStyle;
    styleTransparentContentEven: TcxStyle;
    styleTransparentContentOdd: TcxStyle;
    styleTransparentSelection: TcxStyle;
  end;

implementation

{$R *.dfm}

uses
  cxStyleSheetsLoad;

type
  TdxPScxGridLnkPredefinedStyleSheets = class(TcxPredefinedStyleSheets)
  private
    FDataModule: TdxdmPScxGridLnkPredefinedStyles;
  protected
    procedure AddStyleSheets; override;
    property DataModule: TdxdmPScxGridLnkPredefinedStyles read FDataModule;
  public
    constructor Create; override;
    destructor Destroy; override;

    class procedure Register;
    class procedure Unregister;
  end;

constructor TdxPScxGridLnkPredefinedStyleSheets.Create;
begin
  inherited;
  FDataModule := TdxdmPScxGridLnkPredefinedStyles.Create(nil);
  AddStyleSheets;
end;

destructor TdxPScxGridLnkPredefinedStyleSheets.Destroy;
begin
  FDataModule.Free;
  inherited;
end;

class procedure TdxPScxGridLnkPredefinedStyleSheets.Register;
begin
  cxStyleSheetsLoad.RegisterPredefinedStyleSheets(Self);
end;

class procedure TdxPScxGridLnkPredefinedStyleSheets.Unregister;
begin
  cxStyleSheetsLoad.UnregisterPredefinedStyleSheets(Self);
end;

procedure TdxPScxGridLnkPredefinedStyleSheets.AddStyleSheets;
var
  I: Integer;
begin
  with DataModule.StyleRepository do
    for I := 0 to StyleSheetCount - 1 do
      AddStyleSheet(StyleSheets[I]);
end;

initialization
  TdxPScxGridLnkPredefinedStyleSheets.Register;

finalization
  TdxPScxGridLnkPredefinedStyleSheets.Unregister;

end.
