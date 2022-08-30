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

unit dxRichEdit.Import.OpenXML.DestinationWebSettings;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  SysUtils, Classes,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Import.OpenXML,
  dxXMLReader,
  dxRichEdit.Utils.Types,
  dxGenerics;

type

  { TdxDocumentWebSettingsDestination }

  TdxDocumentWebSettingsDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    class function OnDivsCollection(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  end;

  { TdxDivsDestination }

  TdxDivsDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    class function OnDiv(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  end;

  { TdxDivDestination }

  TdxDivDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FIsBodyDiv: Boolean;
    FId: Integer;
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FTopMargin: Integer;
    FBottomMargin: Integer;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDivDestination; static;
    class function OnBodyDiv(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLeftMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRightMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTopMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBottomMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;

    property IsBodyDiv: Boolean read FIsBodyDiv write FIsBodyDiv;
    property Id: Integer read FId write FId;
    property LeftMargin: Integer read FLeftMargin write FLeftMargin;
    property RightMargin: Integer read FRightMargin write FRightMargin;
    property TopMargin: Integer read FTopMargin write FTopMargin;
    property BottomMargin: Integer read FBottomMargin write FBottomMargin;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxPropertyDestination }

  TdxPropertyDestination = class(TdxLeafElementDestination)
  strict private
    FSetter: TdxAction<Integer>;
  public
    constructor Create(AImporter: TdxWordProcessingMLBaseImporter; const ASetter: TdxAction<Integer>);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

implementation

uses
  Contnrs,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.UnitConverter;

{ TdxDocumentWebSettingsDestination }

class constructor TdxDocumentWebSettingsDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDocumentWebSettingsDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDocumentWebSettingsDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('divs', OnDivsCollection);
end;

class function TdxDocumentWebSettingsDestination.OnDivsCollection(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDivsDestination.Create(AImporter);
end;

function TdxDocumentWebSettingsDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

{ TdxDivsDestination }

class constructor TdxDivsDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDivsDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDivsDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('div', OnDiv);
end;

class function TdxDivsDestination.OnDiv(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDivDestination.Create(AImporter);
end;

function TdxDivsDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

{ TdxDivDestination }

class constructor TdxDivDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDivDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDivDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('bodyDiv', OnBodyDiv);
  Result.Add('marLeft', OnLeftMargin);
  Result.Add('marRight', OnRightMargin);
  Result.Add('marTop', OnTopMargin);
  Result.Add('marBottom', OnBottomMargin);
end;

class function TdxDivDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDivDestination;
begin
  if AImporter.DestinationStack.Count = 0 then
    Result := nil
  else
    Result := TdxDivDestination(AImporter.DestinationStack.Peek);
end;

class function TdxDivDestination.OnBodyDiv(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ASetter: TdxAction<Integer>;
begin
  ASetter := procedure (const AValue: Integer)
    begin
      GetThis(AImporter).IsBodyDiv := AValue <> 0;
    end;
  Result := TdxPropertyDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), ASetter);
end;

class function TdxDivDestination.OnLeftMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ASetter: TdxAction<Integer>;
begin
  ASetter := procedure (const AValue: Integer)
    begin
      GetThis(AImporter).LeftMargin := AValue;
    end;
  Result := TdxPropertyDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), ASetter);
end;

class function TdxDivDestination.OnRightMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ASetter: TdxAction<Integer>;
begin
  ASetter := procedure (const AValue: Integer)
    begin
      GetThis(AImporter).RightMargin := AValue;
    end;
  Result := TdxPropertyDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), ASetter);
end;

class function TdxDivDestination.OnTopMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ASetter: TdxAction<Integer>;
begin
  ASetter := procedure (const AValue: Integer)
    begin
      GetThis(AImporter).TopMargin := AValue;
    end;
  Result := TdxPropertyDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), ASetter);
end;

class function TdxDivDestination.OnBottomMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ASetter: TdxAction<Integer>;
begin
  ASetter := procedure (const AValue: Integer)
    begin
      GetThis(AImporter).BottomMargin := AValue;
    end;
  Result := TdxPropertyDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), ASetter);
end;

function TdxDivDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxDivDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FId := TdxWordProcessingMLBaseImporter(Importer).GetWpSTIntegerValue(AReader, 'id', -1);
end;

procedure TdxDivDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AWebSettings: TdxWebSettings;
  AUnitConverter: TdxDocumentModelUnitConverter;
begin
  if not IsBodyDiv then
    Exit;
  AWebSettings := DocumentModel.WebSettings;
  AUnitConverter := DocumentModel.UnitConverter;
  AWebSettings.LeftMargin := AUnitConverter.TwipsToModelUnits(LeftMargin);
  AWebSettings.RightMargin := AUnitConverter.TwipsToModelUnits(RightMargin);
  AWebSettings.TopMargin := AUnitConverter.TwipsToModelUnits(TopMargin);
  AWebSettings.BottomMargin := AUnitConverter.TwipsToModelUnits(BottomMargin);
end;

{ TdxPropertyDestination }

constructor TdxPropertyDestination.Create(AImporter: TdxWordProcessingMLBaseImporter; const ASetter: TdxAction<Integer>);
begin
  inherited Create(AImporter);
  FSetter := ASetter;
end;

procedure TdxPropertyDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: string;
begin
  AValue := AReader.GetAttribute('val', AReader.NamespaceURI);
  if AValue = '' then
    AValue := AReader.GetAttribute('val');

  if AValue = '' then
    Exit;

  try
    FSetter(StrToInt(AValue));
  except
  end;
end;


end.
