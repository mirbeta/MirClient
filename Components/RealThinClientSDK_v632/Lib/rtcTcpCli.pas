{
  @html(<b>)
  TCP/IP Client Connection
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  Introducing the @html(<b>) @Link(TRtcTcpClient) @html(</b>) component:
  @html(<br>)
  Client connection component for pure TCP/IP communication using raw data.
  There will be no special pre-set formatting when sending or receiving
  data through this client connection component.
  
  @exclude
}
unit rtcTcpCli;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTypes,
  rtcConn,

  rtcSockets,
  rtcSocketCliProv; // Socket Client Provider

type
  { @Abstract(Client Connection component for TCP/IP communication using raw data)

    There is no predefined formatting when sending and receiving
    data through @Link(TRtcTcpClient) connection component.
    Everything that comes through the connection, will be
    received exactly as it was sent (byte-wise). The same goes
    for sending data out through the component. This makes the
    component universal, so it can be used to write virtualy
    any TCP/IP Client application.

    But ... in order for the component to work on all platforms,
    you need to call the "NeedMoreData" method when you are waiting
    for data from the Server. Without expecting
    @html(<br><br>)

    Properties to check first:
    @html(<br>)
    @Link(TRtcConnection.ServerAddr) - Address to connect to
    @html(<br>)
    @Link(TRtcConnection.ServerPort) - Port to connect to
    @html(<br><br>)

    Methods to check first:
    @html(<br>)
    @Link(TRtcClient.Connect) - Connect to server
    @html(<br>)
    @Link(TRtcConnection.Write) - Write data (send to server)
    @html(<br>)
    @Link(TRtcConnection.Read) - Read data (get from server)
    @html(<br>)
    @Link(TRtcConnection.Disconnect) - Disconnect from server
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - Connected to server
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent (buffer now empty)
    @html(<br>)
    @Link(TRtcConnection.OnDataReceived) - Data received (need to read)
    @html(<br>)
    @Link(TRtcConnection.OnDisconnect) - Disconnected from server
    @html(<br><br>)

    @html(<b>function ReadEx:RtcByteArray;</b><br>)
      Use ReadEx to get all the data that is waiting
      for you in this connection component's receiving buffer.
      A call to Read will also clear the buffer, which means that
      you have to store the RtcString received from Read, before
      you start to process it.
      @html(<br><br>)

      Keep in mind that when using TCP/IP,
      data is received as a stream (or rather peaces of it),
      without special end-marks for each package sent or received. This
      means that the server could have sent a big chunk of data in just
      one call, but the client will receive several smaller packages of
      different sizes. It could also happen that server sends multiple
      smaller packages, which your client receives as one big package.
      A combination of those circumstances is also possible.

      @html(<br><br>)
      So, before you start processing the data you receive, make sure that
      you have received everything you need. You could define a buffer for
      storing temporary data, so you can react on multiple OnDataReceived
      events and put all data received inside your buffer, before you
      actually start processing the data.
      @html(<br><br>)

      IMPORTANT: ONLY CALL Read from OnDataReceived event handler.
      OnDataReceived will be automatically triggered by the connection
      component, every time new data becomes available. Please also read
      the explanation for the "NeedMoreData" method if your code also
      has to work on non-Windows platforms.
      @html(<br><br>)

    @html(<b>procedure WriteEx(const s:RtcByteArray);</b><br>)
      Use WriteEx to send data out. Write will not block your
      code execution, it will only put the RtcString into sending buffer
      and return immediatelly.
      @html(<br><br>)

      Keep in mind that all the data you
      put into the sending buffer will remain there until it was
      sent out or a connection was closed. To avoid filling your
      buffer with data that will not be sent out for some time,
      try sending a small peace at a time and then react on the
      @Link(TRtcConnection.OnDataSent) event to continue and send the next one.
      Packages you send out at once shouldn't be larger that 64 KB.
      @html(<br><br>)

    Check @Link(TRtcClient) and @Link(TRtcConnection) for more info.
    }
  TRtcTcpClient = class(TRtcClient)
  protected
    { Creates a new connection provider
      @exclude}
    function CreateProvider:TObject; override;
    // @exclude
    procedure SetParams; override;
  public
    { Use this class function to create a new TRtcTcp_Client
      component at runtime. DO NOT USE the Create constructor. }
    class function New:TRtcTcpClient;

    { In order to make your implementation cross-platform, you need to call
      this method to signal that you are waiting for data from the Server.
      Only after calling "NeedMoreData" will the low-level TCP socket class
      enter a "reading loop" and start waiting for incoming data. Without
      calling "NeedMoreData", the "OnDataReceived" event will NOT be called
      even if the Server has sent data to the Client. }
    procedure NeedMoreData;

  published
    { This event will be triggered every time this connection component's
      buffer is completely empty and the other side has just become ready to
      accept new data. It is good to wait for this event before starting
      to send data out, even though you can start sending data directly
      from the @Link(TRtcConnection.OnConnect) event.
      @html(<br><br>)

      By responding to this event and sending the data only after it was
      triggered, you avoid keeping the data in the send buffer, especially
      if the data you are sending is being read from a file on disk,
      which wouldn't occupy any memory until loaded. }
    property OnReadyToSend;
    { This event will be triggered every time a chunk of your data
      prepared for sending has just been sent out. To know
      exactly how much of it is on the way, use the @Link(TRtcConnection.DataOut) property.
      @html(<br><br>)

      NOTE: Even though data has been sent out, it doesn't mean that
      the other side already received it. It could also be that connection will
      break before this package reaches the other end. }
    property OnDataOut;
    { This event will be triggered when all data prepared for sending
      has been sent out and the sending buffer has become empty again.
      @html(<br><br>)

      When sending large data blocks, try slicing them in small chunks,
      sending a chunk at a time and responding to this event to prepare
      and send the next chunk. This will keep your memory needs low. }
    property OnDataSent;
    { When this event triggers, it means that the other side has sent you
      some data and you can now read it. Check the connection component's
      description to see which properties and methods you can use
      to read the data received. }
    property OnDataReceived;

    { You can set all timeout parameters for the clients underlying API connection or
      default timeout periods for all client connections of the server connection component
      using this property. Check @Link(TRtcTimeoutsOfAPI) for more information. }
    property TimeoutsOfAPI;
    end;

implementation

type
  TSockProv = TRtcSocketClientProvider;

class function TRtcTcpClient.New: TRtcTcpClient;
  begin
  Result:=Create(nil);
  end;

function TRtcTcpClient.CreateProvider:TObject;
  begin
  if not assigned(Con) then
    begin
    Con:=TSockProv.Create;
    TSockProv(Con).SocketClass:=DefaultRtcSocketClass;
    TSockProv(Con).Proto:=proTCP;
    SetTriggers;
    end;
  Result:=Con;
  end;

procedure TRtcTcpClient.SetParams;
  begin
  inherited;
  if assigned(Con) then
    TSockProv(Con).TimeoutsOfAPI:=TimeoutsOfAPI;
  end;

procedure TRtcTcpClient.NeedMoreData;
  begin
  if assigned(Con) then
    TSockProv(Con).NeedMoreData;
  end;

end.

