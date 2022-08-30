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

unit dxRichEdit.DocumentModel.RichEditDocumentServer;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, Forms,

  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Import.Core,
  dxRichEdit.Export.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.BatchUpdateHelper;

type
  { IdxRichEditDocumentServer }

  IdxRichEditDocumentServer = interface(IdxRichEditDocumentContainer)
  ['{A5B76611-FBB7-422D-A7A0-7F5ECEA3D4E4}']
    function GetDpiX: Single;
    function GetDpiY: Single;
    function GetModified: Boolean;
    function GetText: string;
    function GetDocumentModel: TdxDocumentModel;
    function GetMeasurementUnit: TdxMeasurementUnit;
    function GetLayoutUnit: TdxDocumentLayoutUnit;
    procedure SetModified(AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetMeasurementUnit(AValue: TdxMeasurementUnit);
    procedure SetLayoutUnit(AValue: TdxDocumentLayoutUnit);

    procedure AddAfterExportHandler(const AHandler: TNotifyEvent);
    procedure RemoveAfterExportHandler(const AHandler: TNotifyEvent);

    procedure AddSelectionChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveSelectionChangedHandler(const AHandler: TNotifyEvent);

    procedure AddDocumentLoadedHandler(const AHandler: TNotifyEvent);
    procedure RemoveDocumentLoadedHandler(const AHandler: TNotifyEvent);

    procedure AddEmptyDocumentCreatedHandler(const AHandler: TNotifyEvent);
    procedure RemoveEmptyDocumentCreatedHandler(const AHandler: TNotifyEvent);

    procedure AddDocumentClosingHandler(const AHandler: TCloseQueryEvent);
    procedure RemoveDocumentClosingHandler(const AHandler: TCloseQueryEvent);

    procedure AddContentChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveContentChangedHandler(const AHandler: TNotifyEvent);

    procedure AddModifiedChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveModifiedChangedHandler(const AHandler: TNotifyEvent);

    procedure AddUnitChangingHandler(const AHandler: TNotifyEvent);
    procedure RemoveUnitChangingHandler(const AHandler: TNotifyEvent);

    procedure AddUnitChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveUnitChangedHandler(const AHandler: TNotifyEvent);

    procedure AddCalculateDocumentVariableHandler(const AHandler: TdxCalculateDocumentVariableEvent);
    procedure RemoveCalculateDocumentVariableHandler(const AHandler: TdxCalculateDocumentVariableEvent);

    procedure AddBeforeImportHandler(const AHandler: TdxBeforeImportEvent);
    procedure RemoveBeforeImportHandler(const AHandler: TdxBeforeImportEvent);

    procedure AddBeforeExportHandler(const AHandler: TdxBeforeExportEvent);
    procedure RemoveBeforeExportHandler(const AHandler: TdxBeforeExportEvent);

    procedure AddInitializeDocumentHandler(const AHandler: TdxEvent);
    procedure RemoveInitializeDocumentHandler(const AHandler: TdxEvent);

    procedure AddRtfTextChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveRtfTextChangedHandler(const AHandler: TNotifyEvent);
    function GetRtfText: string;
    procedure SetRtfText(const Value: string);

    property RtfText: string read GetRtfText write SetRtfText;
{$REGION other formats}
    procedure AddHtmlTextChangedHandler(const AHandler: TNotifyEvent);
    procedure RemoveHtmlTextChangedHandler(const AHandler: TNotifyEvent);
    function GetHtmlText: string;
    procedure SetHtmlText(const Value: string);

    property HtmlText: string read GetHtmlText write SetHtmlText;
{$ENDREGION}

    function CreateNewDocument(ARaiseDocumentClosing: Boolean): Boolean;
    procedure LoadDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);
    procedure SaveDocument(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);
    procedure LoadDocumentTemplate(AStream: TStream; ADocumentFormat: TdxRichEditDocumentFormat);

    property DpiX: Single read GetDpiX;
    property DpiY: Single read GetDpiY;
    property Modified: Boolean read GetModified write SetModified;
    property Text: string read GetText write SetText;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property MeasurementUnit: TdxMeasurementUnit read GetMeasurementUnit write SetMeasurementUnit;
    property LayoutUnit: TdxDocumentLayoutUnit read GetLayoutUnit write SetLayoutUnit;
  end;

implementation

end.
