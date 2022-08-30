{===============================================================================
  RzSndMsg Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzSendMessage
    Non-visual component that wraps functionality of Simple Messaging API (MAPI)
    allowing a user to send email messages using a MAPI compliant mail service.


  Modification History
  ------------------------------------------------------------------------------
  6.1.10 (05 Sep 2014)
    * Added RequestReceipt property to TRzSendMessage.
  ------------------------------------------------------------------------------
  5.5.1  (31 Mar 2011)
    * Resolved potential range check errors.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Fixed problem where setting ResolveNames to False would result in an
      undeliverable email message.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Added call to Logoff in destructor of TRzSendMessage. This call is now
      required by Outlook in order to successfully terminate the MAPI session
      even when the default session is used.
    * Added new ResolveNames property to TRzSendMessage. When this property is
      set to True, the Recipients properties should contain valid email
      addresses and not names that are to be looked up in the user's address
      book. Setting this property to False avoids the prompt from Outlook
      requesting access to the address book. The default is True, since this is
      how the component originally operated.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Added the MapiInstalled function to TRzSendMessage. This method checks the
      system's Registry to determine if Windows considers the Messaging API
      supported.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Modified the Send method so that directory that is current before the call
      to Send is restored at the end of the Send method.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Attachments are no longer allocated a space in the message text for
      positioning.
===============================================================================}

{$I RzComps.inc}

unit RzSndMsg;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Mapi,
  RzCommon;

type
  EMapiUserAbort = class( EAbort );                         { Silent Exception }
  EMapiError = class( Exception )
    ErrorCode: Cardinal;
  end;

  TRzSendMessage = class( TComponent )
  private
    FAboutInfo: TRzAboutInfo;
    FAttachments: TStrings;
    FToRecipients: TStrings;
    FCcRecipients: TStrings;
    FBccRecipients: TStrings;
    FMessageMemo: TCustomMemo;
    FMessageText: TStrings;
    FPassword: string;
    FProfileName: string;
    FReview: Boolean;
    FSession: LHANDLE;
    FSubject: string;
    FSubjectEdit: TCustomEdit;
    FResolveNames: Boolean;
    FRequestReceipt: Boolean;
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    { Property Access Methods }
    procedure SetAttachments( Value: TStrings ); virtual;
    procedure SetToRecipients( Value: TStrings ); virtual;
    procedure SetCcRecipients( Value: TStrings ); virtual;
    procedure SetBccRecipients( Value: TStrings ); virtual;
    procedure SetMailMessage( Value: TStrings ); virtual;
    procedure SetMessageMemo( Value: TCustomMemo ); virtual;
    procedure SetSubjectEdit( Value: TCustomEdit ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure Logon;
    procedure Logoff;
    procedure Send;
    procedure ClearLists;

    function MapiInstalled: Boolean;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Attachments: TStrings
      read FAttachments
      write SetAttachments;

    property CcRecipients: TStrings
      read FCcRecipients
      write SetCcRecipients;

    property BccRecipients: TStrings
      read FBccRecipients
      write SetBccRecipients;

    property MessageMemo: TCustomMemo
      read FMessageMemo
      write SetMessageMemo;

    property MessageText: TStrings
      read FMessageText
      write SetMailMessage;

    property ProfileName: string
      read FProfileName
      write FProfileName;

    property Password: string
      read FPassword
      write FPassword;

    property Review: Boolean
      read FReview
      write FReview
      default True;

    property ResolveNames: Boolean
      read FResolveNames
      write FResolveNames
      default True;

    property RequestReceipt: Boolean
      read FRequestReceipt
      write FRequestReceipt
      default False;

    property ToRecipients: TStrings
      read FToRecipients
      write SetToRecipients;

    property Subject: string
      read FSubject
      write FSubject;

    property SubjectEdit: TCustomEdit
      read FSubjectEdit
      write SetSubjectEdit;
  end;



implementation

uses
  AnsiStrings,
  Registry;

resourcestring
  sRzMapiUserAbort                  = 'User Abort';
  sRzMapiFailure                    = 'General Failure';
  sRzMapiLoginFailure               = 'Login Failure';
  sRzMapiDiskFull                   = 'Disk Full';
  sRzMapiInsufficientMemory         = 'Insufficient Memory';
  sRzMapiAccessDenied               = 'Access Denied';
  sRzMapiTooManySessions            = 'Too Many Sessions';
  sRzMapiTooManyFiles               = 'Too Many Files';
  sRzMapiTooManyRecipients          = 'Too Many Recipients';
  sRzMapiAttachmentNotFound         = 'Attachment Not Found';
  sRzMapiAttachmentOpenFailure      = 'Attachment Open Failure';
  sRzMapiAttachmentWriteFailure     = 'Attachment Write Failure';
  sRzMapiUnknownRecipient           = 'Unknown Recipient';
  sRzMapiBadRecipType               = 'Bad Recipient Type';
  sRzMapiNoMessages                 = 'No Messages';
  sRzMapiInvalidMessage             = 'Invalid Message';
  sRzMapiTextTooLarge               = 'Text Too Large';
  sRzMapiInvalidSession             = 'Invalid Session';
  sRzMapiTypeNotSupported           = 'Type Not Supported';
  sRzMapiAmbiguousRecipient         = 'Ambiguous Recipient';
  sRzMapiMessageInUse               = 'Message In Use';
  sRzMapiNetworkFailure             = 'Network Failure';
  sRzMapiInvalidEditFields          = 'Invalid Edit Fields';
  sRzMapiInvalidRecips              = 'Invalid Recipients';
  sRzMapiNotSupported               = 'Feature Not Supported';


function CreateMapiError( ErrCode: Cardinal ): Exception;
var
  S: string;
begin
  if ErrCode = mapi_User_Abort then    { If user abort, raise silent exception }
  begin
    Result := EMapiUserAbort.Create( sRzMapiUserAbort );
  end
  else
  begin
    case ErrCode of
      1: S := sRzMapiUserAbort;
      2: S := sRzMapiFailure;
      3: S := sRzMapiLoginFailure;
      4: S := sRzMapiDiskFull;
      5: S := sRzMapiInsufficientMemory;
      6: S := sRzMapiAccessDenied;
      8: S := sRzMapiTooManySessions;
      9: S := sRzMapiTooManyFiles;
      10: S := sRzMapiTooManyRecipients;
      11: S := sRzMapiAttachmentNotFound;
      12: S := sRzMapiAttachmentOpenFailure;
      13: S := sRzMapiAttachmentWriteFailure;
      14: S := sRzMapiUnknownRecipient;
      15: S := sRzMapiBadRecipType;
      16: S := sRzMapiNoMessages;
      17: S := sRzMapiInvalidMessage;
      18: S := sRzMapiTextTooLarge;
      19: S := sRzMapiInvalidSession;
      20: S := sRzMapiTypeNotSupported;
      21: S := sRzMapiAmbiguousRecipient;
      22: S := sRzMapiMessageInUse;
      23: S := sRzMapiNetworkFailure;
      24: S := sRzMapiInvalidEditFields;
      25: S := sRzMapiInvalidRecips;
      26: S := sRzMapiNotSupported;
      else
        S := 'Unknown Error';
    end;

    Result := EMapiError.CreateFmt( 'MAPI: %s. ErrorCode = %d', [ S, ErrCode ] );
    EMapiError( Result ).ErrorCode := ErrCode;
  end;
end;



{&RT}
{============================}
{== TRzSendMessage Methods ==}
{============================}

constructor TRzSendMessage.Create( AOwner: TComponent );
begin
  inherited;
  FSession := 0;
  FAttachments := TStringList.Create;
  FToRecipients := TStringList.Create;
  FCcRecipients := TStringList.Create;
  FBccRecipients := TStringList.Create;
  FMessageText := TStringList.Create;
  FReview := True;
  FResolveNames := True;
  {&RCI}
end;


destructor TRzSendMessage.Destroy;
begin
  Logoff;
  FAttachments.Free;
  FToRecipients.Free;
  FCcRecipients.Free;
  FBccRecipients.Free;
  FMessageText.Free;
  inherited;
end;


{= TRzSendMessage.Notification                                                =}
{=   This method is overridden to ensure that FMemo is set to nil if the      =}
{=   corresponding Memo component is deleted from the form. This method       =}
{=   should be used whenever a contains a reference to another component.     =}

procedure TRzSendMessage.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FMessageMemo then
      FMessageMemo := nil
    else if AComponent = FSubjectEdit then
      FSubjectEdit := nil;
  end;
end;


function TRzSendMessage.MapiInstalled: Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKeyReadOnly( '\SOFTWARE\Microsoft\Windows Messaging Subsystem' ) then
        Result := Reg.ReadString( 'MAPI' ) = '1';
    finally
      Reg.Free;
    end;
  except
  end;
end;



procedure TRzSendMessage.SetAttachments( Value: TStrings );
begin
  FAttachments.Assign( Value );
end;


procedure TRzSendMessage.SetToRecipients( Value: TStrings );
begin
  FToRecipients.Assign( Value );
end;


procedure TRzSendMessage.SetCcRecipients( Value: TStrings );
begin
  FCcRecipients.Assign( Value );
end;


procedure TRzSendMessage.SetBccRecipients( Value: TStrings );
begin
  FBccRecipients.Assign( Value );
end;


procedure TRzSendMessage.SetMailMessage( Value: TStrings );
begin
  FMessageText.Assign( Value );
end;


procedure TRzSendMessage.SetMessageMemo( Value: TCustomMemo );
begin
  FMessageMemo := Value;
  if FMessageMemo <> nil then
    FMessageMemo.FreeNotification( Self );
end;


procedure TRzSendMessage.SetSubjectEdit( Value: TCustomEdit );
begin
  FSubjectEdit := Value;
  if FSubjectEdit <> nil then
    FSubjectEdit.FreeNotification( Self );
end;


procedure TRzSendMessage.Logon;
var
  RetCode: Cardinal;
  Flags: Longint;
begin
  if FProfileName = '' then
    Flags := mapi_Logon_UI or mapi_Dialog or mapi_Use_Default
  else
    Flags := mapi_Logon_UI or mapi_Dialog;

  RetCode := MapiLogon( 0, PAnsiChar( AnsiString( FProfileName ) ),
                        PAnsiChar( AnsiString( FPassword ) ), Flags, 0, @FSession );
  if RetCode > 0 then
    raise CreateMapiError( RetCode );
end;


procedure TRzSendMessage.Logoff;
var
  RetCode: Cardinal;
begin
  if FSession = 0 then
    Exit;

  RetCode := MapiLogoff( FSession, 0, 0, 0 );
  if RetCode > 0 then
    raise CreateMapiError( RetCode );
  FSession := 0;
end;


{ Supporting Constants and Types used to cast generic pointers }
const
  MaxNumFiles = 65520 div SizeOf( TMapiFileDesc );
  MaxNumRecips = 65520 div SizeOf( TMapiRecipDesc );

type
  TFileArray = array[ 0..MaxNumFiles - 1 ] of TMapiFileDesc;
  TRecipArray = array[ 0..MaxNumRecips - 1 ] of TMapiRecipDesc;
  TRecipBufArray = array[ 0..MaxNumRecips - 1 ] of PMapiRecipDesc;


procedure TRzSendMessage.Send;
var
  Files: Pointer;
  FilesMemSize: Word;
  FullPath: string;
  TempStz: array[ 0..255 ] of AnsiChar;
  RecipClass: Longint;
  Recips: Pointer;
  RecipsMemSize: Word;
  RecipBuffer: Pointer;
  RecipBufMemSize: Word;
  RecipName: array[ 0..255 ] of AnsiChar;
  Msg: TMapiMessage;
  RetCode: Cardinal;
  I, J: Integer;
  SendFlags, ResolveFlags: Longint;
  CurrentDir: string;
begin
  {&RV}
  Screen.Cursor := crHourGlass;
  CurrentDir := GetCurrentDir;
  try
    if FSession = 0 then                   { If not yet logged on, then log on }
      Logon;

    FillChar( Msg, SizeOf( TMapiMessage ), 0 );      { Clear out Msg structure }

    if FMessageMemo <> nil then               { If component linked to memo... }
      FMessageText.Assign( FMessageMemo.Lines );           { use memo contents }


    Msg.lpszNoteText := PAnsiChar( AnsiString( FMessageText.Text ) );    { Populate message body }

    if FSubjectEdit <> nil then
      Msg.lpszSubject := PAnsiChar( AnsiString( FSubjectEdit.Text ) )
    else
      Msg.lpszSubject := PAnsiChar( AnsiString( FSubject ) );

    { Add Recipients to Message }
                                            { Specify the number of recipients }
    Msg.nRecipCount := FToRecipients.Count + FCcRecipients.Count + FBccRecipients.Count;

    RecipsMemSize := SizeOf( TMapiRecipDesc ) * Msg.nRecipCount;
    Recips := nil;
    RecipBufMemSize := SizeOf( PMapiRecipDesc ) * Msg.nRecipCount;
    RecipBuffer := nil;

    if Msg.nRecipCount > 0 then
    begin
      GetMem( Recips, RecipsMemSize );        { Allocate Memory for Recips Array }
      FillChar( Recips^, RecipsMemSize, 0 );

      GetMem( RecipBuffer, RecipBufMemSize );  { Allocate Memory for RecipBuffer }

      for I := 0 to Msg.nRecipCount - 1 do
      begin
        if I < FToRecipients.Count then
        begin
          {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrPCopy( RecipName, AnsiString( FToRecipients[ I ] ) );
          RecipClass := mapi_TO;
        end
        else if I < FToRecipients.Count + FCcRecipients.Count then
        begin
          {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrPCopy( RecipName, AnsiString( FCcRecipients[ I - FToRecipients.Count ] ) );
          RecipClass := mapi_CC;
        end
        else
        begin
          {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrPCopy( RecipName, AnsiString( FBccRecipients[ I - FToRecipients.Count - FCcRecipients.Count ] ) );
          RecipClass := mapi_BCC;
        end;

        if FResolveNames then
        begin
          { Call MapiResolveName to get the Address for the RecipName recipient }
          if FReview then
            ResolveFlags := mapi_Logon_UI + mapi_Dialog
          else
            ResolveFlags := mapi_Logon_UI;

          RetCode := MapiResolveName( FSession, 0, RecipName, ResolveFlags, 0,
                                      TRecipBufArray( RecipBuffer^ )[ I ] );

          if RetCode > 0 then
          begin
            { Free up previous recipients }
            for J := 0 to I - 1 do
              MapiFreeBuffer( TRecipBufArray( RecipBuffer^ )[ J ] );
            raise CreateMapiError( RetCode );
          end;

          { Populate Recips array with data retrieved in RecipBuffer }

          TRecipArray( Recips^ )[ I ].ulReserved := TRecipBufArray( RecipBuffer^ )[ I ]^.ulReserved;
          TRecipArray( Recips^ )[ I ].ulRecipClass := RecipClass;
          TRecipArray( Recips^ )[ I ].lpszName := TRecipBufArray( RecipBuffer^ )[ I ]^.lpszName;
          TRecipArray( Recips^ )[ I ].lpszAddress := TRecipBufArray( RecipBuffer^ )[ I ]^.lpszAddress;
          TRecipArray( Recips^ )[ I ].ulEIDSize := TRecipBufArray( RecipBuffer^ )[ I ]^.ulEIDSize;
          TRecipArray( Recips^ )[ I ].lpEntryID := TRecipBufArray( RecipBuffer^ )[ I ]^.lpEntryID;
        end
        else // Do not resolve names - assume recipients are standard email addresses
        begin
          TRecipArray( Recips^ )[ I ].ulRecipClass := RecipClass;
          TRecipArray( Recips^ )[ I ].lpszName := {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrNew( {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrPCopy( TempStz, RecipName ) );
          TRecipArray( Recips^ )[ I ].lpszAddress := {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrNew( {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrPCopy( TempStz, AnsiString( 'SMTP:' + RecipName ) ) );
        end;
      end;
      Msg.lpRecips := Recips;
    end;

    { Add Attachments to Message }

    Msg.nFileCount := FAttachments.Count;  { Specify the number of attachments }
    Files := nil;
    FilesMemSize := SizeOf( TMapiFileDesc ) * FAttachments.Count;
    if Msg.nFileCount > 0 then
    begin
      GetMem( Files,  FilesMemSize );
      FillChar( Files^, FilesMemSize, 0 );

      for I := 0 to FAttachments.Count - 1 do
      begin
        FullPath := ExpandFileName( FAttachments[ I ] );
        TFileArray(Files^)[ I ].lpszPathName := {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrNew( {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrPCopy( TempStz, AnsiString( FullPath ) ) );

        // $FFFFFFFF indicates that attachments are not positioned within the message text
        TFileArray( Files^ )[ I ].nPosition := $FFFFFFFF;
      end;
      Msg.lpFiles := Files;
    end;

    { Send the Message }
    try
      if FReview or ( FToRecipients.Count = 0 ) then
        SendFlags := mapi_Logon_UI + mapi_Dialog     { Show the compose dialog }
      else
        SendFlags := mapi_Logon_UI;

      if FRequestReceipt then
        SendFlags := SendFlags or mapi_Receipt_Requested;

      { MapiSendMail will attempt to use the session established by MapiLogon }
      { or if FSession is 0, it will use a shared session, and if one is not  }
      { available, a new session is started.                                  }

      RetCode := MapiSendMail( FSession, Application.Handle, Msg, SendFlags, 0 );
      if RetCode > 0 then
        raise CreateMapiError( RetCode );
    finally
      { Clean Up:  Lots of dynamic memory to free up }

      if Msg.nRecipCount > 0 then
      begin
        for I := 0 to FToRecipients.Count + FCcRecipients.Count + FBccRecipients.Count - 1 do
        begin
          if not FResolveNames then
          begin
            {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrDispose( TRecipArray( Recips^ )[ I ].lpszName );
            {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrDispose( TRecipArray( Recips^ )[ I ].lpszAddress );
          end;
          MapiFreeBuffer( TRecipBufArray( RecipBuffer^ )[ I ] );
        end;
        FreeMem( Recips, RecipsMemSize );
        FreeMem( RecipBuffer, RecipBufMemSize );
      end;

      if Msg.nFileCount > 0 then
      begin
        for I := 0 to FAttachments.Count - 1 do
          {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrDispose( TFileArray( Files^ )[ I ].lpszPathName );
        FreeMem( Files, FilesMemSize );
      end;
    end;
  finally
    SetCurrentDir( CurrentDir );
    Screen.Cursor := crDefault;
  end;
end;

procedure TRzSendMessage.ClearLists;
begin
  FAttachments.Clear;
  FToRecipients.Clear;
  FCcRecipients.Clear;
  FBccRecipients.Clear;
  FMessageText.Clear;
end;

{&RUIF}
end.
