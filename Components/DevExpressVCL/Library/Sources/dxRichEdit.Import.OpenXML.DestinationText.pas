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

unit dxRichEdit.Import.OpenXML.DestinationText;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxXMLReader,
  dxRichEdit.Utils.Types,
  dxRichEdit.Import.OpenXML.DestinationBase;

type

  { TdxTextDestination }

  TdxTextDestination = class(TdxElementDestination)
  strict private
    const
      TrimChars: array[0..3] of char = (' ', #$000A, #$000D, #$0009);
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    function RemoveRedundantSpaces(const AText: string): string; virtual;
  public
    function ProcessText(AReader: TdxXmlReader): Boolean; override;
  end;

implementation

uses
  Math,
  dxRichEdit.Utils.Exceptions,
  dxStringHelper;

{ TdxTextDestination }

class constructor TdxTextDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTextDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTextDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
end;

function TdxTextDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxTextDestination.ProcessText(AReader: TdxXmlReader): Boolean;
var
  AText, APlainText: string;
begin
  AText := AReader.Value;
  if AReader.XmlSpace <> TdxXmlSpace.Preserve then
    AText := RemoveRedundantSpaces(AText);
  if AText <> '' then
  begin
    APlainText := TdxStringHelper.ReplaceParagraphMarksWithLineBreaks(AText);
    if APlainText <> '' then
      Importer.PieceTable.InsertTextCore(Importer.Position, APlainText);
  end;
  Result := True;
end;

function TdxTextDestination.RemoveRedundantSpaces(const AText: string): string;
var
  P: PChar;
begin
  Result := TdxStringHelper.Trim(AText, TrimChars);
  P := PChar(Result);
  UniqueString(Result);
  while P^ <> #0 do
  begin
    if (P^ = #$0009) or (P^ = #$000A) then
      P^ :=  #$0020;
    Inc(P);
  end;
end;

end.

