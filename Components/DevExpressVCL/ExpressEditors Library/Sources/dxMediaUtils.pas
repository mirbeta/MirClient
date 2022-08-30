{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxMediaUtils;

{$I cxVer.inc}

interface

uses
  Windows, ActiveX, ComObj;

{$IFNDEF VER220}
(*$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumMoniker);'*)
{$ENDIF}

const
  VMRMode_Windowless = 2;
  EC_DEVICE_LOST = $1f;

  IID_ICaptureGraphBuilder2: TGUID = '{93E5A4E0-2D50-11d2-ABFA-00A0C9C6E38D}';
  IID_IGraphBuilder: TGUID = '{56A868A9-0AD4-11CE-B03A-0020AF0BA770}';
  IID_ICreateDevEnum: TGUID = '{29840822-5B84-11D0-BD3B-00A0C911CE86}';
  IID_IMediaControl: TGUID = '{56A868B1-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IMediaEventEx: TGUID = '{56A868C0-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IBaseFilter: TGUID = '{56A86895-0AD4-11CE-B03A-0020AF0BA770}';
  IID_ISampleGrabber: TGUID = '{6B652FFF-11FE-4FCE-92AD-0266B5D7C78F}';
  IID_IVMRFilterConfig: TGUID = '{9E5530C5-7034-48B4-BB46-0B8A6EFC8E36}';
  IID_IVMRWindowlessControl: TGUID = '{0EB1088C-4DCD-46F0-878F-39DAE86A51B7}';
  IID_IVideoWindow: TGUID = '{56A868B4-0AD4-11CE-B03A-0020AF0BA770}';
  IID_IAMStreamConfig: TGUID = '{C6E13340-30AC-11D0-A18C-00A0C9118956}';
  IID_IVMRImageCompositor: TGUID = '{7A4FB5Af-479F-4074-BB40-CE6722E43C82}';
  IID_IAMCopyCaptureFileProgress: TGUID = '{670D1D20-A068-11D0-B3F0-00AA003761C5}';
  IID_IPin: TGUID = '{56A86891-0Ad4-11CE-B03A-0020AF0BA770}';

  CLSID_VideoInputDeviceCategory: TGUID = '{860BB310-5D01-11d0-BD3B-00A0C911CE86}';
  CLSID_SystemDeviceEnum: TGUID = '{62BE5D10-60EB-11d0-BD3B-00A0C911CE86}';
  CLSID_CaptureGraphBuilder2: TGUID = '{BF87B6E1-8C27-11d0-B3F0-00AA003761C5}';
  CLSID_FilterGraph: TGUID = '{E436EBB3-524F-11CE-9F53-0020AF0BA770}';
  CLSID_SampleGrabber: TGUID = '{C1F400A0-3F08-11d3-9F0B-006008039E37}';
  CLSID_NullRenderer: TGUID = '{C1F400A4-3F08-11D3-9F0B-006008039E37}';
  CLSID_VideoMixingRenderer: TGUID = '{B87BEB7B-8D29-423F-AE4D-6582C10175AC}';

  FORMAT_VideoInfo: TGUID = '{05589F80-C356-11CE-BF01-00AA0055595A}';
  MEDIATYPE_Video: TGUID = '{73646976-0000-0010-8000-00AA00389B71}';
  MEDIASUBTYPE_RGB32: TGUID = '{E436EB7E-524F-11CE-9F53-0020AF0BA770}';
  MEDIASUBTYPE_AVI: TGUID = '{E436EB88-524F-11CE-9F53-0020AF0BA770}';
  PIN_CATEGORY_CAPTURE: TGUID = '{FB6C4281-0353-11D1-905F-0000C0CC16BA}';
  PIN_CATEGORY_PREVIEW: TGUID = '{FB6C4282-0353-11D1-905F-0000C0CC16BA}';

type

  TVideoStreamConfigCaps = record
    guid: TGUID;
    VideoStandard: ULONG;
    InputSize: TSize;
    MinCroppingSize: TSize;
    MaxCroppingSize: TSize;
    CropGranularityX: Integer;
    CropGranularityY: Integer;
    CropAlignX: Integer;
    CropAlignY: Integer;
    MinOutputSize: TSize;
    MaxOutputSize: TSize;
    OutputGranularityX: Integer;
    OutputGranularityY: Integer;
    StretchTapsX: Integer;
    StretchTapsY: Integer;
    ShrinkTapsX: Integer;
    ShrinkTapsY: Integer;
    MinFrameInterval: Int64;
    MaxFrameInterval: Int64;
    MinBitsPerSecond: Longint;
    MaxBitsPerSecond: Longint;
  end;

  TPinDirection = (PINDIR_INPUT, PINDIR_OUTPUT);

  PReferenceTime = ^LONGLONG;

  PAMMediaType = ^TAMMediaType;
  _AMMediaType = record
    majortype: TGUID;
    subtype: TGUID;
    bFixedSizeSamples: BOOL;
    bTemporalCompression: BOOL;
    lSampleSize: ULONG;
    formattype: TGUID;
    pUnk: IUnknown;
    cbFormat: ULONG;
    pbFormat: Pointer;
  end;
  TAMMediaType = _AMMediaType;

  TVideoInfoHeader = record
    rcSource: TRect;
    rcTarget: TRect;
    dwBitRate: DWORD;
    dwBitErrorRate: DWORD;
    AvgTimePerFrame: LONGLONG;
    bmiHeader: TBitmapInfoHeader;
  end;

  IMediaFilter = interface(IPersist)
  ['{56A86899-0AD4-11CE-B03A-0020AF0BA770}']
  end;

  IBaseFilter = interface(IMediaFilter)
  ['{56A86895-0AD4-11CE-B03A-0020AF0BA770}']
  end;

  IEnumFilters = interface(IUnknown)
    ['{56A86893-0AD4-11CE-B03A-0020AF0BA770}']
    function Next(cFilters: ULONG; out ppFilter: IBaseFilter; pcFetched: PULONG): HResult; stdcall;
    function Skip(cFilters: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
  end;

  IFilterGraph = interface(IUnknown)
  ['{56A8689F-0AD4-11CE-B03A-0020AF0BA770}']
    function AddFilter(pFilter: IBaseFilter; pName: PWideChar): HResult; stdcall;
    function RemoveFilter(pFilter: IBaseFilter): HResult; stdcall;
    function EnumFilters(out ppEnum: IEnumFilters): HResult; stdcall;
  end;

  IFileSinkFilter = interface(IUnknown)
    ['{A2104830-7C70-11CF-8BCE-00AA00A3F1A6}']
    function GetCurFile(out ppszFileName: PWideChar; pmt: PAMMediaType): HResult; stdcall;
    function SetFileName(pszFileName: PWideChar; pmt: PAMMediaType): HResult; stdcall;
  end;

  IGraphBuilder = interface(IFilterGraph)
  ['{56A868A9-0AD4-11CE-B03A-0020AF0BA770}']
    function SetFiltergraph(pfg: IGraphBuilder): HResult; stdcall;
    function GetFiltergraph(out ppfg: IGraphBuilder): HResult; stdcall;
    function SetOutputFileName(const pType: TGUID; lpstrFile: PWCHAR; out ppf: IBaseFilter; out ppSink: IFileSinkFilter): HResult; stdcall;
  end;

  IAMCopyCaptureFileProgress = interface
    ['{670D1D20-A068-11D0-B3F0-00AA003761C5}']
  end;

  IPin = interface(IUnknown)
    ['{56A86891-0Ad4-11CE-B03A-0020AF0BA770}']
    function Connect(pReceivePin: IPin; const pmt: PAMMediaType): HResult; stdcall;
    function ReceiveConnection(pConnector: IPin; const pmt: TAMMediaType): HResult; stdcall;
    function Disconnect: HResult; stdcall;
    // other metods
  end;

  ICaptureGraphBuilder2 = interface(IUnknown)
  ['{93E5A4E0-2D50-11d2-ABFA-00A0C9C6E38D}']
    function SetFiltergraph(pfg: IGraphBuilder): HResult; stdcall;
    function GetFiltergraph(out ppfg: IGraphBuilder): HResult; stdcall;
    function SetOutputFileName(const pType: TGUID; lpstrFile: PWideChar; out ppf: IBaseFilter; out ppSink: IFileSinkFilter): HResult; stdcall;
    function FindInterface(pCategory, pType: PGUID; pf: IBaseFilter; const riid: TGUID; out ppint): HResult; stdcall;
    function RenderStream(pCategory, pType: PGUID; pSource: IUnknown; pIntermediate, pSink: IBaseFilter): HResult; stdcall;
    function ControlStream(pCategory, pType: PGUID; pFilter: IBaseFilter; pstart, pstop: PReferenceTime; wStartCookie, wStopCookie: WORD ): HResult; stdcall;
    function AllocCapFile(lpwstr: PWCHAR; dwlSize: Int64): HResult; stdcall;
    function CopyCaptureFile(lpwstrOld, lpwstrNew: PWCHAR; fAllowEscAbort: Integer; pCallback: IAMCopyCaptureFileProgress): HResult; stdcall;
    function FindPin(pSource: IUnknown; pindir: TPinDirection; pCategory, pType: PGUID; fUnconnected: BOOL; num: Integer; out ppPin: IPin): HResult; stdcall;
  end;

  ISampleGrabber = interface(IUnknown) // use IMediaSample instead
  ['{6B652FFF-11FE-4FCE-92AD-0266B5D7C78F}']
    function SetOneShot(OneShot: BOOL): HResult; stdcall;
    function SetMediaType(const pType: TAMMediaType): HResult; stdcall;
    function GetConnectedMediaType(out pType: TAMMediaType): HResult; stdcall;
    function SetBufferSamples(BufferThem: BOOL): HResult; stdcall;
    function GetCurrentBuffer(var pBufferSize: Integer; pBuffer: Pointer): HResult; stdcall;
    // other metods
  end;

  IMediaControl = interface(IDispatch)
  ['{56A868B1-0AD4-11CE-B03A-0020AF0BA770}']
    function Run: HResult; stdcall;
    function Pause: HResult; stdcall;
    function Stop: HResult; stdcall;
    // other metods
  end;

  IMediaEvent = interface(IDispatch)
  ['{56A868B6-0AD4-11CE-B03A-0020AF0BA770}']
    function GetEventHandle(out hEvent: Longint): HResult; stdcall;
    function GetEvent(out lEventCode, lParam1, lParam2: Longint; msTimeout: DWORD): HResult; stdcall;
    function WaitForCompletion(msTimeout: DWORD; out pEvCode: Longint): HResult; stdcall;
    function CancelDefaultHandling(lEvCode: Longint): HResult; stdcall;
    function RestoreDefaultHandling(lEvCode: Longint): HResult; stdcall;
    function FreeEventParams(lEvCode, lParam1, lParam2: Longint): HResult; stdcall;
  end;

  IMediaEventEx = interface(IMediaEvent)
  ['{56A868C0-0AD4-11CE-B03A-0020AF0BA770}']
    function SetNotifyWindow(hwnd: HWND; lMsg, lInstanceData: Longint): HResult; stdcall;
    // other metods
  end;

  ICreateDevEnum = interface(IUnknown)
  ['{29840822-5B84-11D0-BD3B-00A0C911CE86}']
    function CreateClassEnumerator(const clsidDeviceClass: TGUID; out ppEnumMoniker: IEnumMoniker; dwFlags: DWORD): HRESULT; stdcall;
  end;

  IVideoWindow = interface(IDispatch)
  ['{56A868B4-0AD4-11CE-B03A-0020AF0BA770}']
    function put_Caption(strCaption: WideString): HResult; stdcall;
    function get_Caption(out strCaption: WideString): HResult; stdcall;
    function put_WindowStyle(WindowStyle: Longint): HResult; stdcall;
    function get_WindowStyle(out pWindowStyle: Longint): HResult; stdcall;
    function put_WindowStyleEx(WindowStyleEx: Longint): HResult; stdcall;
    function get_WindowStyleEx(out pWindowStyleEx: Longint): HResult; stdcall;
    function put_AutoShow(AutoShow: LongBool): HResult; stdcall;
    function get_AutoShow(out AutoShow: LongBool): HResult; stdcall;
    function put_WindowState(WindowState: Longint): HResult; stdcall;
    function get_WindowState(out WindowState: Longint): HResult; stdcall;
    function put_BackgroundPalette(BackgroundPalette: Longint): HResult; stdcall;
    function get_BackgroundPalette(out pBackgroundPalette: Longint): HResult; stdcall;
    function put_Visible(Visible: LongBool): HResult; stdcall;
    function get_Visible(out pVisible: LongBool): HResult; stdcall;
    function put_Left(Left: Longint): HResult; stdcall;
    function get_Left(out pLeft: Longint): HResult; stdcall;
    function put_Width(Width: Longint): HResult; stdcall;
    function get_Width(out pWidth: Longint): HResult; stdcall;
    function put_Top(Top: Longint): HResult; stdcall;
    function get_Top(out pTop: Longint): HResult; stdcall;
    function put_Height(Height: Longint): HResult; stdcall;
    function get_Height(out pHeight: Longint): HResult; stdcall;
    function put_Owner(Owner: Longint): HResult; stdcall;
    function get_Owner(out pOwner: Longint): HResult; stdcall;
    function put_MessageDrain(Drain: Longint): HResult; stdcall;
    function get_MessageDrain(out Drain: Longint): HResult; stdcall;
    function get_BorderColor(out pColor: Longint): HResult; stdcall;
    function put_BorderColor(Color: Longint): HResult; stdcall;
    function get_FullScreenMode(out FullScreenMode: LongBool): HResult; stdcall;
    function put_FullScreenMode(FullScreenMode: LongBool): HResult; stdcall;
    function SetWindowForeground(Focus: Longint): HResult; stdcall;
    function NotifyOwnerMessage(hwnd: Longint; uMsg, wParam, lParam: Longint): HResult; stdcall;
    function SetWindowPosition(Left, Top, Width, Height: Longint): HResult; stdcall;
    // other metods
  end;

  IVMRWindowlessControl = interface(IUnknown)
  ['{0EB1088C-4DCD-46F0-878F-39DAE86A51B7}']
    function GetNativeVideoSize(out lpWidth, lpHeight, lpARWidth, lpARHeigh: Longint): HResult; stdcall;
    function GetMinIdealVideoSize(out lpWidth, lpHeight: Longint): HResult; stdcall;
    function GetMaxIdealVideoSize(out lpWidth, lpHeight: Longint): HResult; stdcall;
    function SetVideoPosition(lpSRCRect, lpDSTRect: PRect): HResult; stdcall;
    function GetVideoPosition(out lpSRCRect, lpDSTRect: TRect): HResult; stdcall;
    function GetAspectRatioMode(out lpAspectRatioMode: DWORD): HResult; stdcall;
    function SetAspectRatioMode(AspectRatioMode: DWORD): HResult; stdcall;
    function SetVideoClippingWindow(hwnd: HWND): HResult; stdcall;
    // other metods
  end;

  IVMRImageCompositor = interface(IUnknown)
  ['{7A4FB5AF-479F-4074-BB40-CE6722E43C82}']
  end;

  IVMRFilterConfig = interface(IUnknown)
  ['{9E5530C5-7034-48B4-BB46-0B8A6EFC8E36}']
    function SetImageCompositor(lpVMRImgCompositor: IVMRImageCompositor): HResult; stdcall;
    function SetNumberOfStreams(dwMaxStreams: DWORD): HResult; stdcall;
    function GetNumberOfStreams(out pdwMaxStreams: DWORD): HResult; stdcall;
    function SetRenderingPrefs(dwRenderFlags: DWORD): HResult; stdcall;
    function GetRenderingPrefs(out pdwRenderFlags: DWORD): HResult; stdcall;
    function SetRenderingMode(pMode: DWORD): HResult; stdcall;
    // other metods
  end;

  IAMStreamConfig = interface(IUnknown)
  ['{C6E13340-30AC-11D0-A18C-00A0C9118956}']
    function SetFormat(const pmt: TAMMediaType): HResult; stdcall;
    function GetFormat(out ppmt: PAMMediaType): HResult; stdcall;
    function GetNumberOfCapabilities(out piCount, piSize: Integer): HResult; stdcall;
    function GetStreamCaps(iIndex: Integer; out pmt: PAMMediaType; out pSCC): HResult; stdcall;
  end;

implementation

end.
