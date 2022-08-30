{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Control.Reg;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

procedure Register;

implementation

uses
  SysUtils, VCLEditors, DesignIntf, DesignEditors, DesignMenus, Classes,
  dxCore, dxCoreReg, dxBuiltInPopupMenu, cxClasses,
  cxLibraryReg, cxPC, cxEditPropEditors, dxUIGeneratorDesignHelpers,

  dxRichEdit.Utils.Types,
  dxEncoding,
  dxMeasurementUnitEdit,
  dxSymbolListBox,
  dxContainerListBox,
  dxRichEditFontNameComboBox,
  dxRichEditDialogsSimpleControl,
  dxRichEdit.Options,
  dxRichEdit.Control,
  dxRichEditBorderLineWeightComboBox;

const
  dxRichEditControlProductName  = 'ExpressRichEditControl Suite';

type
  { TdxRichEditControlComponentEditor }

  TdxRichEditControlComponentEditor = class(TdxUIGeneratorComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  { TdxRichEditControlSelectionEditor }

  TdxRichEditControlSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxOptionsEncodingPropertyEditor }

  TdxOptionsEncodingPropertyEditor = class(TIntegerProperty)
  strict private
    FDisplayNames: TStringList;
    function CreateDisplayNames: TStringList;
  strict protected
    property DisplayNames: TStringList read FDisplayNames;
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEncoding: Word; virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxImportOptionsEncodingPropertyEditor }

  TdxImportOptionsEncodingPropertyEditor = class(TdxOptionsEncodingPropertyEditor)
  strict private
    function GetOptions: TdxDocumentImporterOptions;
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;

    property Options: TdxDocumentImporterOptions read GetOptions;
  end;

  { TdxExportOptionsEncodingPropertyEditor }

  TdxExportOptionsEncodingPropertyEditor = class(TdxOptionsEncodingPropertyEditor)
  strict private
    function GetOptions: TdxDocumentExporterOptions;
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;

    property Options: TdxDocumentExporterOptions read GetOptions;
  end;

{ TdxRichEditControlComponentEditor }

function TdxRichEditControlComponentEditor.GetProductName: string;
begin
  Result := dxRichEditControlProductName;
end;

{ TdxRichEditControlSelectionEditor }

procedure TdxRichEditControlSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  Proc('dxCore');
  Proc('dxCoreClasses');
  Proc('cxGraphics');
  Proc('dxGDIPlusAPI');
  Proc('dxGDIPlusClasses');
  Proc('dxRichEdit.NativeApi');
  Proc('dxRichEdit.Types');
  Proc('dxRichEdit.Options');
  Proc('dxRichEdit.Control');
  Proc('dxRichEdit.Control.SpellChecker');
  Proc('dxRichEdit.Dialogs.EventArgs');
  if not TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard then
    Proc(cxGetUnitName(TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass));
end;

{ TdxOptionsEncodingPropertyEditor }

constructor TdxOptionsEncodingPropertyEditor.Create(const ADesigner: IDesigner; APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  FDisplayNames := CreateDisplayNames;
end;

function TdxOptionsEncodingPropertyEditor.CreateDisplayNames: TStringList;
var
  AEncodings: TArray<TEncoding>;
  AEncoding: TEncoding;
begin
  Result := TStringList.Create;
  AEncodings := TdxEncoding.Encodings;
  for AEncoding in AEncodings do
    Result.AddObject(AEncoding.DisplayName, AEncoding);
  Result.Sort;
end;

destructor TdxOptionsEncodingPropertyEditor.Destroy;
begin
  FDisplayNames.Free;
  inherited Destroy;
end;

function TdxOptionsEncodingPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable];
end;

procedure TdxOptionsEncodingPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to FDisplayNames.Count - 1 do
    Proc(FDisplayNames[I]);
end;

{ TdxImportOptionsEncodingPropertyEditor }

function TdxImportOptionsEncodingPropertyEditor.GetOptions: TdxDocumentImporterOptions;
begin
  Result := Safe<TdxDocumentImporterOptions>.Cast(GetComponent(0));
end;

function TdxImportOptionsEncodingPropertyEditor.GetValue: string;
begin
  if Options <> nil then
  begin
    Result := Options.ActualEncoding.DisplayName;
    if Options.IsDefaultEncoding then
      Result := Format('%s - Default', [Result]);
  end
  else
    Result := '';
end;

procedure TdxImportOptionsEncodingPropertyEditor.SetValue(const Value: string);
var
  AIndex: Integer;
begin
  if Options <> nil then
  begin
    AIndex := DisplayNames.IndexOf(Value);
    if (AIndex >= 0) and (Options.ActualEncoding <> TEncoding(DisplayNames.Objects[AIndex])) then
    begin
      Options.ActualEncoding := TEncoding(DisplayNames.Objects[AIndex]);
      Modified;
    end;
  end;
end;

{ TdxExportOptionsEncodingPropertyEditor }

function TdxExportOptionsEncodingPropertyEditor.GetOptions: TdxDocumentExporterOptions;
begin
  Result := Safe<TdxDocumentExporterOptions>.Cast(GetComponent(0));
end;

function TdxExportOptionsEncodingPropertyEditor.GetValue: string;
begin
  if Options <> nil then
  begin
    Result := Options.ActualEncoding.DisplayName;
    if Options.IsDefaultEncoding then
      Result := Format('%s - Default', [Result]);
  end
  else
    Result := '';
end;

procedure TdxExportOptionsEncodingPropertyEditor.SetValue(const Value: string);
var
  AIndex: Integer;
begin
  if Options <> nil then
  begin
    AIndex := DisplayNames.IndexOf(Value);
    if (AIndex >= 0) and (Options.ActualEncoding <> TEncoding(DisplayNames.Objects[AIndex])) then
    begin
      Options.ActualEncoding := TEncoding(DisplayNames.Objects[AIndex]);
      Modified;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents(dxCoreLibraryProductPage, [TdxRichEditControl]);
  RegisterComponentEditor(TdxRichEditControl, TdxRichEditControlComponentEditor);
  RegisterPropertyEditor(TypeInfo(TShortCut), TdxHyperlinkOptions, 'ModifierKeys', TShortCutProperty);
  RegisterPropertyEditor(TypeInfo(Word), TdxDocumentImporterOptions, 'Encoding', TdxImportOptionsEncodingPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Word), TdxDocumentExporterOptions, 'Encoding', TdxExportOptionsEncodingPropertyEditor);

  RegisterNoIcon([TdxMeasurementUnitEdit, TdxSymbolListBox, TdxSimpleSymbolListBox, TdxContainerListBox,
    TdxSimpleRichEditControl, TdxRichEditFontNameComboBox, TdxBorderLineWeightComboBox]);
  RegisterSelectionEditor(TdxRichEditControl, TdxRichEditControlSelectionEditor);
end;

end.
