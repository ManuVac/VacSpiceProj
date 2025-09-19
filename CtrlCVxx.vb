Imports Microsoft.VisualBasic.VBMath

Public Class CtrlCVxx
#Region "variables"
    'shared event 
    Friend Shared Event ObjClicked(ByVal index As Int16) 'pour remonter le numero de l'index de l'objet
    Friend Shared Event ObjRemoved(ByVal index As Int16) 'pour suprimer l'obj
    Friend Shared Event ObjCopy(ByVal obj As Int16) 'pour copier l'obj
    Friend Shared Event mymouseleave()

    '' Friend Shared Event AnchorPos(ByVal MyPoints() As Point, ByVal MyPointIn As Point, ByVal MyPointOut As Point, ByVal mypointtop As Point, ByVal mypointBot As Point, index As Int16) ' remonte la position du milier de la bride d'entrée
    Friend Shared Event ObjMooving(ByVal index As Int16) ' remonte le numero de l'obj en mouvement
    Friend Shared Event ButtonRealease(ByVal release As Boolean, ByVal indexCH As Int16)  'remonte l'info pour dire que j'ai relaché l'objet
    'point
    Private Off As Point
    'for color pen
    Private pen_color As Pen = Pens.Red
    ' for drawing line
    Private Plot_X1 As Single
    Private Plot_Y1 As Single
    Private Plot_X2 As Single
    Private Plot_Y2 As Single
    Dim mypos As New Point(120, 120)
    Private PointMilieuLink(5) As Point
    Private MyLinkPointPanel(10) As Panel

    Private v_mousemouve As Boolean 'si la mouse est en mouvement

    Private Const ConstanteOutGasingBake As Double = 0.00000000001
    Private Const ConstanteOutGasingUnBake As Double = 0.0000000003
    Private Const ConstanteNegPumpingSpeed As Double = 0.001 'pour 1cm2

    Structure sSpiceModel
        Dim R_in As String
        Dim R_out As String
        Dim R_up As String
        Dim R_down As String
        Dim R_ip As String
        Dim I As String
        Dim C As String
    End Structure
    Structure sVacuumModel
        Dim C_in As Double 'conductance 
        Dim C_out As Double 'conductance 
        Dim C_up As Double 'conductance 
        Dim C_down As Double 'conductance 
        Dim Conductance As Double ' conductance generale
        '' Dim Volume As Double 'volume de la chambre
        ''Dim Neg As Double   'vitesse de pompage en l/s
        '' Dim Q As Double     'degasage
    End Structure
    Structure sPulse
        Dim V1 As String        'inital voltage
        Dim V2 As String        'peak voltage
        Dim TD As String        'inital delay time
        Dim Tr As String        'Rise time
        Dim Tf As String        'Fall time
        Dim PW As String        'pulse wise
        Dim Periode As String   'period
    End Structure
#End Region
#Region "enum"
    Enum eRegime
        turbulent = 1
        laminaire = 2
        transitoire = 4
        moleculaire = 5
        surface = 6
    End Enum
    Enum eBaked
        baked = 0
        Unbaked = 1
        other = 2
    End Enum

    Enum eTypObj
        CH = 1
        IP = 2
        PEN = 3
        Node = 4
        Start = 5
        End_ = 6
        IPs = 7
        IPe = 8
        IPw = 9
        NodeNE = 10
        NodeSE = 11
        NodeWS = 12
        NodeNW = 13
        PENs = 14
        PENw = 15
        PENe = 16
        CHph = 17
        CHpb = 18
        CHphb = 19
        IPx = 20
        GAZ = 21
        Volume1 = 22
        NodeX = 23

    End Enum
   
    Enum eFamily
        CH = 1
        IP = 2
        PEN = 3
        NODE = 4
        START = 5
        END_ = 6
        GAZ = 7
        VOLUME = 8
    End Enum
#End Region
#Region "Property"
    Private v_x_in As Double
    Public Property X_in() As Double
        Get
            Return v_x_in
        End Get
        Set(ByVal value As Double)
            v_x_in = value
        End Set
    End Property

    Private v_x_out As Double
    Public Property X_out() As Double
        Get
            Return v_x_out
        End Get
        Set(ByVal value As Double)
            v_x_out = value
        End Set
    End Property

    Private v_y_up As Double
    Public Property Y_up() As Double
        Get
            Return v_y_up
        End Get
        Set(ByVal value As Double)
            v_y_up = value
        End Set
    End Property

    Private v_y_down As Double
    Public Property Y_down() As Double
        Get
            Return v_y_down
        End Get
        Set(ByVal value As Double)
            v_y_down = value
        End Set
    End Property

    Private v_vacuumModelArray() As sVacuumModel
    Public Property VacuumModelArray() As sVacuumModel()

        Get
            Return v_vacuumModelArray
        End Get
        Set(ByVal value As sVacuumModel())
            v_vacuumModelArray = value
        End Set
    End Property


    '' Private v_vacuummodel As sVacuumModel
    ''  Public Property VacuumModel() As sVacuumModel
    '      Get
    '        Return v_vacuummodel
    '    End Get
    '   Set(ByVal value As sVacuumModel)
    '        v_vacuummodel = value
    '  End Set
    ' End Property

    Private v_tran As Boolean
    Public Property Transien() As Boolean
        Get
            Return v_tran
        End Get
        Set(ByVal value As Boolean)
            v_tran = value
        End Set
    End Property



    Private v_spicemodel As sSpiceModel
    Public Property SpiceModel() As sSpiceModel
        Get




            Return v_spicemodel
        End Get
        Set(ByVal value As sSpiceModel)
            v_spicemodel = value
        End Set
    End Property


    Private v_volumez As Double
    Public Property volumez() As Double
        Get
            Return v_volumez
        End Get
        Set(ByVal value As Double)
            v_volumez = value
        End Set
    End Property


    Private v_volumey As Double
    Public Property volumey() As Double
        Get
            Return v_volumey
        End Get
        Set(ByVal value As Double)
            v_volumey = value
        End Set
    End Property



    Private v_modelSpice As String
    Public Property ModelSpice() As String
        Get
            Return v_modelSpice
        End Get
        Set(ByVal value As String)
            v_modelSpice = value
        End Set
    End Property

    'nombre de linkpoint
    Private v_LinkNumber As Int16
    Public Property LinkNumber() As Int16
        Get
            Return v_LinkNumber
        End Get
        Set(ByVal value As Int16)
            v_LinkNumber = value
        End Set
    End Property
    'definition du nom de l'obj 
    Private v_typeObjString As String
    Public ReadOnly Property TypeObjString() As String
        Get
            Return v_typeObjString
        End Get

    End Property
    Private v_family As eFamily
    Public Property Family() As eFamily
        Get
            Return v_family
        End Get
        Set(ByVal value As eFamily)
            v_family = value
        End Set
    End Property


    'force le numero d'index dans la famille.
    Private v_indexFamily As Int16
    Public Property IndexFamily() As Int16
        Get

            Return v_indexFamily
        End Get

        Set(ByVal value As Int16)
            v_indexFamily = value

        End Set
    End Property


    Private v_ConfigObj As eTypObj
    Public Property ConfigObj() As eTypObj
        Get
            Return v_ConfigObj
        End Get
        Set(ByVal value As eTypObj)
            v_ConfigObj = value

            '' info forme

            Me.BackgroundImage = Nothing
            v_typeObjString = "CH"
            'dessin de la cv
            PanelCV.Visible = True
            Panelph.Visible = False
            Panelpb.Visible = False
            'creation des brides de la cv
            PanelBrideDown.Visible = True
            PanelBrideIn.Visible = True
            PanelBrideTop.Visible = False
            PanelbrideBot.Visible = False

            'defini la longeur par defaut
            '' longeur = 3
            'defini le nom
            CvName = "CH" & v_indexFamily
            ''nombre de link point
            LinkNumber = 4
            LinkPoint(0)._Enable = True 'in
            LinkPoint(1)._Enable = True 'out
            LinkPoint(2)._Enable = False 'top
            LinkPoint(3)._Enable = False 'bot
        End Set
    End Property
    Private v_pulse As sPulse
    Public Property Pulse() As sPulse
        Get
            Return v_pulse
        End Get
        Set(ByVal value As sPulse)
            v_pulse = value
        End Set
    End Property

    Private v_objImage As System.Drawing.Image
    Public Property ObjImage() As System.Drawing.Image
        Get
            Return v_objImage
        End Get
        Set(ByVal value As System.Drawing.Image)
            v_objImage = value
        End Set
    End Property


    Private v_typeObj As eTypObj
    Public Property TypeObj() As eTypObj
        Get
            Return v_typeObj
        End Get
        Set(ByVal value As eTypObj)
            Dim i As Int16
            v_typeObj = value
            Select Case value
                Case eTypObj.CH
                    '' info forme

                    Me.BackgroundImage = Nothing
                    v_objImage = My.Resources.CH
                    v_typeObjString = "CH"
                    'dessin de la cv
                    PanelCV.Visible = True
                    Panelph.Visible = False
                    Panelpb.Visible = False
                    'creation des brides de la cv
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = False


                    'defini le nom
                    CvName = "CH" & v_indexFamily
                    ''nombre de link point
                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = False 'bot

                    'vacuum config
                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 300  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    ''  IPpumpingSpeed = 300
                    ''  DiametreIP = 15



                Case eTypObj.CHpb
                    Me.BackgroundImage = Nothing
                    v_typeObjString = "CH"
                    v_objImage = My.Resources.CHpb

                    'dessin de la cv
                    PanelCV.Visible = True
                    Panelph.Visible = False
                    Panelpb.Visible = True
                    'creation des brides de la cv
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = True


                    'defini le nom
                    CvName = "CH" & v_indexFamily
                    ''nombre de link point
                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = True 'bot

                    'vacuum config
                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 300  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    ''  IPpumpingSpeed = 300
                    '' DiametreIP = 15


                Case eTypObj.CHph
                    Me.BackgroundImage = Nothing
                    v_objImage = My.Resources.CHph
                    v_typeObjString = "CH"
                    'dessin de la cv
                    PanelCV.Visible = True
                    Panelph.Visible = True
                    Panelpb.Visible = False
                    'creation des brides de la cv
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = True
                    PanelbrideBot.Visible = False


                    'defini le nom
                    CvName = "CH" & v_indexFamily
                    ''nombre de link point
                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = True 'top
                    LinkPoint(3)._Enable = False 'bot

                    'vacuum config
                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 300  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    '' IPpumpingSpeed = 300
                    ''  DiametreIP = 15

                Case eTypObj.CHphb
                    Me.BackgroundImage = Nothing
                    v_objImage = My.Resources.CHphb
                    v_typeObjString = "CH"
                    'dessin de la cv
                    PanelCV.Visible = True
                    Panelph.Visible = True
                    Panelpb.Visible = True
                    'creation des brides de la cv
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = True
                    PanelbrideBot.Visible = True

                    'defini le nom
                    CvName = "CH" & v_indexFamily
                    ''nombre de link point
                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = True 'top
                    LinkPoint(3)._Enable = True 'bot

                    'vacuum config
                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 300  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    '' IPpumpingSpeed = 300
                    ''  DiametreIP = 15

                Case eTypObj.End_
                    v_typeObjString = "END"
                    'cache/affiche les obj non utile
                    Me.BackgroundImage = My.Resources.End_
                    v_objImage = Me.BackgroundImage
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = False
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = False


                    'defini le nom
                    LabelCVName.Dock = DockStyle.Left
                    CvName = "END" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = False 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = False 'bot

                    'vacuum config
                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 0    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    ''  IPpumpingSpeed = 300
                    '' DiametreIP = 15

                Case eTypObj.IP
                    v_typeObjString = "IP"
                    Me.BackgroundImage = My.Resources.IP
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = False
                    PanelBrideIn.Visible = False
                    PanelBrideTop.Visible = True
                    PanelbrideBot.Visible = False


                    'defini le nom
                    LabelCVName.Dock = DockStyle.Bottom
                    CvName = "IP" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = False 'in
                    LinkPoint(1)._Enable = False 'out
                    LinkPoint(2)._Enable = True 'top
                    LinkPoint(3)._Enable = False 'bot

                    'vacuum config
                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    pumpingSpeed = 240
                    V_volume = 18.6


                Case eTypObj.IPx
                    v_typeObjString = "IP"
                    Me.BackgroundImage = My.Resources.IPx
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = False


                    'defini le nom
                    LabelCVName.Dock = DockStyle.Bottom
                    CvName = "IP" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = False 'bot

                    'vacuum config
                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    pumpingSpeed = 240
                    V_volume = 18.6

                Case eTypObj.IPe
                    v_typeObjString = "IP"
                    Me.BackgroundImage = My.Resources.IPe
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = False
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = False


                    'defini le nom
                    LabelCVName.Dock = DockStyle.Bottom
                    CvName = "IP" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = False 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = False 'bot

                    'vacuum config
                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    pumpingSpeed = 240
                    V_volume = 18.6


                Case eTypObj.IPs
                    v_typeObjString = "IP"
                    Me.BackgroundImage = My.Resources.IPs
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = False
                    PanelBrideIn.Visible = False
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = True


                    'defini le nom
                    LabelCVName.Dock = DockStyle.Top
                    CvName = "IP" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = False 'in
                    LinkPoint(1)._Enable = False 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = True 'bot

                    'vacuum config
                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    pumpingSpeed = 240
                    V_volume = 18.6

                Case eTypObj.IPw
                    v_typeObjString = "IP"
                    Me.BackgroundImage = My.Resources.IPw
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = False
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = False


                    'defini le nom
                    LabelCVName.Dock = DockStyle.Right
                    CvName = "IP" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = False 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = False 'bot

                    'vacuum config
                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 15   'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    pumpingSpeed = 240
                    V_volume = 18.6


                Case eTypObj.Node
                    v_typeObjString = "NODE"
                    Me.BackgroundImage = My.Resources.Node
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = True
                    PanelbrideBot.Visible = True


                    CvName = "N" & v_indexFamily

                    Me.Height = 38
                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = True 'top
                    LinkPoint(3)._Enable = True 'bot

                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    X_in = 20
                    X_out = 20
                    Y_up = 20
                    Y_down = 20
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    ''pumpingSpeed = 300


                Case eTypObj.NodeNE
                    v_typeObjString = "NODEne"
                    Me.BackgroundImage = My.Resources.NodeNE
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = False
                    PanelBrideTop.Visible = True
                    PanelbrideBot.Visible = False

                    Me.Height = 100
                    CvName = "N" & v_indexFamily

                    LinkNumber = 4
                    LinkPoint(0)._Enable = False 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = True 'top
                    LinkPoint(3)._Enable = False 'bot
                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    X_in = 0
                    X_out = 20
                    Y_up = 20
                    Y_down = 0
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    ''pumpingSpeed = 300


                Case eTypObj.NodeSE
                    v_typeObjString = "NODse"
                    Me.BackgroundImage = My.Resources.NodeSE
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = False
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = True

                    Me.Height = 100
                    CvName = "N" & v_indexFamily

                    LinkNumber = 4
                    LinkPoint(0)._Enable = False 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = True 'bot

                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    X_in = 0
                    X_out = 20
                    Y_up = 20
                    Y_down = 0
                    Diametre = 15 'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    ''pumpingSpeed = 300


                Case eTypObj.NodeWS
                    v_typeObjString = "NODEws"
                    Me.BackgroundImage = My.Resources.NodeWS
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = False
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = True

                    Me.Height = 100
                    CvName = "N" & v_indexFamily

                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = False 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = True 'bot
                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    X_in = 20
                    X_out = 0
                    Y_up = 0
                    Y_down = 20
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    ''pumpingSpeed = 300


                Case eTypObj.NodeNW
                    v_typeObjString = "NODEnw"
                    Me.BackgroundImage = My.Resources.NodeNW
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = False
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = True
                    PanelbrideBot.Visible = False

                    Me.Height = 100
                    CvName = "N" & v_indexFamily

                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = False 'out
                    LinkPoint(2)._Enable = True 'top
                    LinkPoint(3)._Enable = False 'bot

                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    X_in = 20
                    X_out = 0
                    Y_up = 20
                    Y_down = 0
                    Diametre = 15    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    ''pumpingSpeed = 300



                Case eTypObj.NodeX
                    v_typeObjString = "NODEx"
                    Me.BackgroundImage = My.Resources.nodesize
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = True
                    PanelbrideBot.Visible = True

                    Me.Height = 48
                    CvName = "N" & v_indexFamily

                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = True 'top
                    LinkPoint(3)._Enable = True 'bot

                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 100  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    X_in = 20
                    X_out = 20
                    Y_up = 20
                    Y_down = 20
                    Diametre = 15   'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    ''pumpingSpeed = 300


                Case eTypObj.PEN
                    v_typeObjString = "PENNING"

                    Me.BackgroundImage = My.Resources.penning
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = False
                    PanelBrideIn.Visible = False
                    PanelBrideTop.Visible = True
                    PanelbrideBot.Visible = False

                    'defini la longeur par defaut

                    'defini le nom
                    LabelCVName.Dock = DockStyle.Bottom
                    CvName = "Pen" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = False 'in
                    LinkPoint(1)._Enable = False 'out
                    LinkPoint(2)._Enable = True 'top
                    LinkPoint(3)._Enable = False 'bot
                    LinkNumber = 4

                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 0    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    pumpingSpeed = 0.1
                    V_volume = 0.02 ' litres

                Case eTypObj.PENs
                    v_typeObjString = "PENNINGs"

                    Me.BackgroundImage = My.Resources.penningS
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = False
                    PanelBrideIn.Visible = False
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = True

                    'defini la longeur par defaut

                    'defini le nom
                    LabelCVName.Dock = DockStyle.Left
                    CvName = "Pen" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = False 'in
                    LinkPoint(1)._Enable = False 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = True 'bot
                    LinkNumber = 4

                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 0    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    pumpingSpeed = 0.1
                    V_volume = 0.02 ' litres

                Case eTypObj.PENe
                    v_typeObjString = "PENNINGe"

                    Me.BackgroundImage = My.Resources.penningE
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = False
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = False

                    'defini le nom
                    LabelCVName.Dock = DockStyle.Left
                    CvName = "Pen" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = False 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = False 'bot
                    LinkNumber = 4

                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 0    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    pumpingSpeed = 0.1
                    V_volume = 0.02 ' litres

                Case eTypObj.PENw
                    v_typeObjString = "PENNINGw"

                    Me.BackgroundImage = My.Resources.penningW
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = False
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = False


                    'defini le nom
                    LabelCVName.Dock = DockStyle.Left
                    CvName = "Pen" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = False 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = False 'bot
                    LinkNumber = 4

                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    ''volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 0    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    pumpingSpeed = 0.1
                    V_volume = 0.02 ' litres

                Case eTypObj.Start

                    v_typeObjString = "START"
                    Me.BackgroundImage = My.Resources.start
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = False
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = False


                    'defini le nom
                    LabelCVName.Dock = DockStyle.Right
                    CvName = "START" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = False 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = False 'bot

                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    ''volumez = 50
                    '' volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 0    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    '' pumpingSpeed = 300


                Case eTypObj.GAZ
                    v_typeObjString = "GAZ"
                    Me.BackgroundImage = My.Resources.Gaz
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = False
                    PanelBrideIn.Visible = False
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = True


                    'defini le nom
                    LabelCVName.Dock = DockStyle.Right

                    CvName = "GAZ" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = False 'in
                    LinkPoint(1)._Enable = False 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = True 'bot

                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80  'longeur de la chambre en mm
                    longeur = 80
                    '' volumez = 50
                    '' volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 0    'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.other
                    '' pumpingSpeed = 300


                Case eTypObj.Volume1
                    v_typeObjString = "Volume"
                    Me.BackgroundImage = My.Resources.volume
                    v_objImage = Me.BackgroundImage
                    'cache/affiche les obj non utile
                    PanelCV.Visible = False
                    PanelBrideDown.Visible = True
                    PanelBrideIn.Visible = True
                    PanelBrideTop.Visible = False
                    PanelbrideBot.Visible = False

                    'defini la longeur par defaut

                    'defini le nom
                    LabelCVName.Dock = DockStyle.Right

                    CvName = "V" & v_indexFamily
                    LinkNumber = 4
                    LinkPoint(0)._Enable = True 'in
                    LinkPoint(1)._Enable = True 'out
                    LinkPoint(2)._Enable = False 'top
                    LinkPoint(3)._Enable = False 'bot

                    'vacuum config

                    Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
                    Location = mypos
                    longeur = 80
                    volumez = 50
                    volumey = 50
                    '' X_in = 20
                    '' X_out = 20
                    '' Y_up = 20
                    '' Y_down = 20
                    Diametre = 15   'diametre de la chambre en mm
                    OutgasingFactor = 0.0000000003
                    OutgasingType = CtrlCVxx.eBaked.Unbaked
                    '' pumpingSpeed = 300


            End Select
        End Set
    End Property



    Private v_reajustLinkpointsPosition(9) As Point
    Public WriteOnly Property reajustLinkPointsPosition() As Point

        Set(ByVal value As Point)
            'recalcule la position des linkPoint apres le relaché de la souris 


            'in

            LinkPoint(0)._Point.X = value.X
            LinkPoint(0)._Point.Y = value.Y + Me.Height / 2
            v_reajustLinkpointsPosition(0) = LinkPoint(0)._Point
            'out
            LinkPoint(1)._Point.X = value.X + Me.Width
            LinkPoint(1)._Point.Y = value.Y + Me.Height / 2
            v_reajustLinkpointsPosition(1) = LinkPoint(1)._Point
            'top
            LinkPoint(2)._Point.X = value.X + Me.Width / 2
            LinkPoint(2)._Point.Y = value.Y
            v_reajustLinkpointsPosition(2) = LinkPoint(2)._Point
            'bot
            LinkPoint(3)._Point.X = value.X + Me.Width / 2
            LinkPoint(3)._Point.Y = value.Y + Me.Height
            v_reajustLinkpointsPosition(3) = LinkPoint(3)._Point

        End Set
    End Property







    'pour chaque point de l'obj nous trouvons ses coordonnées 

    Structure sPoint
        Dim _Point As Point                     '' coordonnée du linkpoint
        Dim _Enable As Boolean                  '' si le linkpoint est actif
        Dim _connected As Boolean               '' si le linkpoint est connecté
        Dim _linkedToObj As Int16               '' numero de l'obj parent connecté
        Dim _linkedToObjName As String          '' nom de l'obj parent connecté
        Dim _linkedToPoint As Int16             '' numero du linkpoint de l'obj parent
        Dim _LinkedToNode As String             '' nom du node qui relie deux obj c'est pour Spice
    End Structure
    Private v_linkPoint(9) As sPoint
    Public Property LinkPoint() As sPoint()

        Get

            Return v_linkPoint
        End Get
        Set(ByVal value As sPoint())


            v_linkPoint = value

        End Set
    End Property
    Private v_surfacearray() As Double
    Public Property SurfaceArray() As Double()
        Get
            Dim i As Integer
            Dim j As Double = v_longeur / v_segmentNbr ' la longueur d' un segment de la ch
            Select Case v_family


                Case eFamily.VOLUME
                    For i = 0 To v_segmentNbr - 1
                        v_surfacearray(i) = (((j * v_volumey) * 2) + ((v_volumez * v_volumey) * 2) + ((v_volumez * j) * 2))
                    Next
                Case eFamily.NODE
                    For i = 0 To v_segmentNbr - 1
                        Dim di As Double = (Math.PI * v_diametre)
                        v_surfacearray(i) = di * (X_in + X_out + Y_up + Y_down)
                    Next
                Case Else ' CH
                    For i = 0 To v_segmentNbr - 1
                        v_surfacearray(i) = (Math.PI * (v_diametre) * (j))
                    Next

            End Select

            Return v_surfacearray
        End Get
        Set(ByVal value As Double())
            v_surfacearray = value
        End Set
    End Property



    Private v_surface As Double
    Public ReadOnly Property Surface() As Double 'surface interieur de la CV cm2
        Get
            Select Case v_family
                Case eFamily.VOLUME

                    v_surface = (((v_longeur * v_volumey) * 2) + ((v_volumez * v_volumey) * 2) + ((v_volumez * v_longeur) * 2))
                Case eFamily.NODE
                    Dim di As Double = (Math.PI * v_diametre)
                    v_surface = di * (X_in + X_out + Y_up + Y_down)
                Case Else
                    v_surface = (Math.PI * (v_diametre) * (v_longeur))
            End Select


            Return v_surface
        End Get

    End Property

    Private v_volumeArray() As Double
    Public Property VolumeArray() As Double()

        Get
            Dim i As Integer
            Dim j As Double = v_longeur / v_segmentNbr ' la longueur d' un segment de la ch
            Select Case Family
                Case eFamily.VOLUME
                    For i = 0 To v_segmentNbr - 1
                        v_volumeArray(i) = (j * v_volumey * v_volumez) / 1000 'l
                    Next
                Case eFamily.NODE ' la segmentation ne s'applique pas au node.
                    For i = 0 To v_segmentNbr - 1
                        v_volumeArray(i) = ((v_x_in * (Math.PI * (v_diametre / 2) ^ 2)) / 1000) + ((v_x_out * (Math.PI * (v_diametre / 2) ^ 2)) / 1000) + ((v_y_up * (Math.PI * (v_diametre / 2) ^ 2)) / 1000) + ((v_y_down * (Math.PI * (v_diametre / 2) ^ 2)) / 1000)
                    Next
                Case eFamily.CH
                    For i = 0 To v_segmentNbr - 1
                        v_volumeArray(i) = (j * (Math.PI * (v_diametre / 2) ^ 2)) / 1000 'l
                    Next

            End Select
            Return v_volumeArray
        End Get
        Set(ByVal value As Double())
            v_volumeArray = value
        End Set
    End Property


    Private V_volume As Double
    'volume de la chambre a vide
    Public ReadOnly Property Volume() As Double 'mm3 --> dm3
        Get
            Select Case Family
                Case eFamily.VOLUME
                    V_volume = (v_longeur * v_volumey * v_volumez) / 1000 'l
                Case eFamily.NODE
                    V_volume = ((v_x_in * (Math.PI * (v_diametre / 2) ^ 2)) / 1000) + ((v_x_out * (Math.PI * (v_diametre / 2) ^ 2)) / 1000) + ((v_y_up * (Math.PI * (v_diametre / 2) ^ 2)) / 1000) + ((v_y_down * (Math.PI * (v_diametre / 2) ^ 2)) / 1000)
                Case eFamily.CH
                    V_volume = (v_longeur * (Math.PI * (v_diametre / 2) ^ 2)) / 1000 'l

            End Select
            '' v_vacuummodel.Volume = V_volume

            Return V_volume
        End Get

    End Property
    Private v_cvnameArray() As String
    Public Property CvNameArray() As String()
        Get
            Dim i As Integer
            For i = 0 To v_segmentNbr - 1
                v_cvnameArray(i) = "[" & i & "]" & v_cvName

            Next
            Return v_cvnameArray
        End Get
        Set(ByVal value As String())
            v_cvnameArray = value
        End Set
    End Property


    Private v_cvName As String
    'nom de la chambre
    Public Property CvName() As String
        Get
            Return v_cvName
        End Get
        Set(ByVal value As String)
            v_cvName = value
            LabelCVName.Text = value
        End Set
    End Property


    Private v_longeur As Double 'longeur de la chambre en m
    Public Property longeur() As Double
        Get
            Return v_longeur
        End Get
        Set(ByVal value As Double)
            Dim temp As Double
            temp = (value * 261) / 600
            Me.Width = temp


            v_longeur = value
        End Set
    End Property

    Private v_segmentNbr As Integer = 1
    ' nombre de segment pour le calcul du profile de pression 1 par defaut
    Public Property SegmentNbr() As Integer
        Get
            Return v_segmentNbr
        End Get
        Set(ByVal value As Integer)
            v_segmentNbr = value

            ReDim ConductanceArray(v_segmentNbr)
            ReDim v_conductanceArray(v_segmentNbr)

            ReDim SurfaceArray(v_segmentNbr)
            ReDim v_surfacearray(v_segmentNbr)

            ReDim OutGasingArray(v_segmentNbr)
            ReDim v_outgasingArray(v_segmentNbr)

            ReDim v_volumeArray(v_segmentNbr)
            ReDim VolumeArray(v_segmentNbr)

            ReDim v_cvnameArray(v_segmentNbr)
            ReDim CvNameArray(v_segmentNbr)




        End Set
    End Property

    Private v_pressureProfile As Boolean
    Public ReadOnly Property PressureProfile() As Boolean
        Get
            Return v_pressureProfile
        End Get

    End Property







    Private v_diametre As Double 'diametre de la chambre en mm et transformée en 
    Public Property Diametre() As Double
        Get


            Return v_diametre
        End Get
        Set(ByVal value As Double)
            v_diametre = value


        End Set
    End Property

    Private v_regime As eRegime
    Public Property Regime() As eRegime
        Get
            Return v_regime
        End Get
        Set(ByVal value As eRegime)
            v_regime = value
        End Set
    End Property

    Private v_p1 As Double 'pression amon mbar 
    Public Property p1() As Double 'pression mbar 
        Get
            Return v_p1
        End Get
        Set(ByVal value As Double) 'pression mbar 
            v_p1 = value
        End Set
    End Property

    Private v_p2 As Double  'pression aval mbar
    Public Property P2() As Double
        Get
            Return v_p2
        End Get
        Set(ByVal value As Double)
            v_p2 = value
        End Set
    End Property

    ' Private v_resistance As Double
    '  Public ReadOnly Property Resistance() As Double
    '      Get
    '      v_resistance = 1 / v_conductance.Conductance
    '        Return v_resistance
    '    End Get

    ' End Property

    Private v_negActive As Boolean ''si la chambre a un depot neg
    Public Property NegActive() As Boolean
        Get
            Return v_negActive
        End Get
        Set(ByVal value As Boolean)

            v_negActive = value
            If value Then
                PanelCV.BackColor = Color.Gray

            Else
                PanelCV.BackColor = Color.Gainsboro

            End If
        End Set
    End Property

    Private v_negPumpingSpeedArray() As Double
    Public ReadOnly Property NegPumpingSpeedArray() As Double()
        Get
            Dim i As Integer
            For i = 0 To v_segmentNbr - 1
                v_negPumpingSpeedArray(i) = ((ConstanteNegPumpingSpeed) * v_surfacearray(i))
            Next
            Return v_negPumpingSpeedArray
        End Get
       
    End Property


    Private v_negPumpingSpeed As Double = 1 '' vitesse de pompage du neg
    Public ReadOnly Property NegPumpingSpeed() As Double
        Get
            v_negPumpingSpeed = ((ConstanteNegPumpingSpeed) * v_surface)
            Return v_negPumpingSpeed
        End Get

    End Property


    Private v_pumpingSpeed As Double = 1
    Public Property pumpingSpeed() As Double
        Get
            Return v_pumpingSpeed
        End Get
        Set(ByVal value As Double)
            v_pumpingSpeed = value

        End Set
    End Property

    Private v_conductanceArray() As sVacuumModel
    Public Property ConductanceArray() As sVacuumModel()
        Get
            Dim i As Integer
            Dim j As Double = v_longeur / v_segmentNbr ' la longueur d' un segment de la ch
            Select Case v_family
                Case eFamily.CH

                    For i = 0 To v_segmentNbr - 1
                        v_conductanceArray(i).C_in = 12.1 * v_diametre ^ 3 / (j / 2)
                        v_conductanceArray(i).C_out = 12.1 * v_diametre ^ 3 / (j / 2)
                        v_conductanceArray(i).C_up = 0
                        v_conductanceArray(i).C_down = 0

                    Next

                Case eFamily.VOLUME

                    For i = 0 To v_segmentNbr - 1
                        v_conductanceArray(i).C_in = 12.1 * v_diametre ^ 3 / (j / 2)
                        v_conductanceArray(i).C_out = 12.1 * v_diametre ^ 3 / (j / 2)
                        v_conductanceArray(i).C_up = 12.1 * v_diametre ^ 3 / (j / 2)
                        v_conductanceArray(i).C_down = 12.1 * v_diametre ^ 3 / (j / 2)

                        v_conductanceArray(i).Conductance = 12.1 * v_diametre ^ 3 / (j)
                    Next
            End Select
            Return v_conductanceArray
        End Get
        Set(ByVal value As sVacuumModel())
            v_conductanceArray = value
        End Set
    End Property





    Private v_conductance As sVacuumModel 'calcule la conductance 

    Public Property Conductance() As sVacuumModel
        'toute les mesures sont en cm ref : https://www.pfeiffer-vacuum.com/en/know-how/introduction-to-vacuum-technology/fundamentals/conductance/
        Get
            Select Case v_regime

                Case eRegime.laminaire
                    v_conductance.Conductance = 1.35 * ((v_diametre ^ 4 * v_p1 + v_p2) / (v_longeur * 2))
                    '' v_conductanceHalf = 1.35 * ((v_diametre ^ 4 * v_p1 + v_p2) / (v_longeur))

                Case eRegime.moleculaire

                    Select Case v_family
                        Case eFamily.CH

                            v_conductance.C_in = 12.1 * v_diametre ^ 3 / (v_longeur / 2)
                            v_conductance.C_out = 12.1 * v_diametre ^ 3 / (v_longeur / 2)
                            v_conductance.C_up = 0
                            v_conductance.C_down = 0

                            v_conductance.Conductance = 12.1 * v_diametre ^ 3 / (v_longeur)
                        Case eFamily.IP
                            Select Case v_typeObj
                                Case eTypObj.IPx
                                    v_conductance.C_in = 11.77 * Math.PI * (v_diametre / 2) ^ 2
                                    v_conductance.C_out = 11.77 * Math.PI * (v_diametre / 2) ^ 2
                                    v_conductance.Conductance = 11.77 * Math.PI * (v_diametre / 2) ^ 2
                                Case Else
                                    '' v_conductance = 11.77 * Math.PI * (v_diametreIP / 2) ^ 2
                            End Select


                        Case eFamily.NODE
                            v_conductance.C_in = 12.1 * v_diametre ^ 3 / (v_x_in)
                            v_conductance.C_out = 12.1 * v_diametre ^ 3 / (v_x_out)
                            v_conductance.C_up = 12.1 * v_diametre ^ 3 / (v_y_up)
                            v_conductance.C_down = 12.1 * v_diametre ^ 3 / (v_y_down)

                            v_conductance.Conductance = 12.1 * v_diametre ^ 3 / (v_x_in + v_x_out)
                        Case eFamily.VOLUME
                            v_conductance.C_in = 12.1 * v_diametre ^ 3 / (v_longeur / 2)
                            v_conductance.C_out = 12.1 * v_diametre ^ 3 / (v_longeur / 2)
                            v_conductance.C_up = 12.1 * v_diametre ^ 3 / (v_longeur / 2)
                            v_conductance.C_down = 12.1 * v_diametre ^ 3 / (v_longeur / 2)

                            v_conductance.Conductance = 12.1 * v_diametre ^ 3 / (v_longeur)


                    End Select


                Case eRegime.surface
                    v_conductance.Conductance = 9999
                Case eRegime.transitoire
                    v_conductance.Conductance = 9999
                Case eRegime.turbulent
                    v_conductance.Conductance = 9999

            End Select

            Return v_conductance 'en l/s
        End Get
        Set(ByVal value As sVacuumModel)
            v_conductance = value
        End Set
    End Property


    Private v_outgasingFactor As Double
    Public Property OutgasingFactor() As Double
        Get
            Return v_outgasingFactor
        End Get
        Set(ByVal value As Double)
            v_outgasingFactor = value
        End Set
    End Property
    Private v_leak As Double
    Public Property Leak() As Double
        Get
            Return v_leak
        End Get
        Set(ByVal value As Double)
            v_leak = value
        End Set
    End Property

    Private v_outgasingArray() As Double
    Public Property OutGasingArray() As Double()
        Get
            Dim i As Integer
            For i = 0 To v_segmentNbr - 1
                v_outgasingArray(i) = v_outgasingFactor * v_surfacearray(i)
                If v_outgasingType = eBaked.other Then v_outgasingArray(i) = v_outgasingFactor

            Next

            Return v_outgasingArray
        End Get
        Set(ByVal value As Double())
            v_outgasingArray = value
        End Set
    End Property


    Private v_outgasing As Double
    Public ReadOnly Property Outgasing() As Double
        Get
            v_outgasing = v_outgasingFactor * v_surface
            If v_outgasingType = eBaked.other Then v_outgasing = v_outgasingFactor
            '' v_vacuummodel.Q = v_outgasing
            Return v_outgasing
        End Get

    End Property

    Private v_outgasingType As eBaked
    Public Property OutgasingType() As eBaked
        Get
            Return v_outgasingType
        End Get
        Set(ByVal value As eBaked)
            v_outgasingType = value

            Select Case value

                Case eBaked.baked
                    v_outgasingFactor = ConstanteOutGasingBake
                Case eBaked.Unbaked
                    v_outgasingFactor = ConstanteOutGasingUnBake

            End Select
        End Set
    End Property






#Region "graphique"




    Private m_SnapToGrid As Boolean = True
    Public Property SnapToGrid() As Boolean
        Get
            Return m_SnapToGrid
        End Get
        Set(ByVal value As Boolean)
            m_SnapToGrid = value
        End Set
    End Property

    Private v_Index As Int16
    Public Property Index() As Int16
        Get
            Return v_Index
        End Get
        Set(ByVal value As Int16)
            v_Index = value
        End Set
    End Property

    Private m_X2
    Public Property X2() As Single
        Get
            Return m_X2
        End Get
        Set(ByVal value As Single)
            m_X2 = value
        End Set
    End Property

    Private m_Y2 As Single
    Public Property Y2() As Single
        Get
            Return m_Y2
        End Get
        Set(ByVal value As Single)
            m_Y2 = value
        End Set
    End Property

    Private m_GridX As Integer = 1
    Public Property GridX() As Integer
        Get
            Return m_GridX
        End Get
        Set(ByVal value As Integer)
            m_GridX = value
        End Set
    End Property

    Private m_GridY As Integer = 1
    Public Property GridY() As Integer
        Get
            Return m_GridY
        End Get
        Set(ByVal value As Integer)
            m_GridY = value
        End Set
    End Property

    Private m_X1 As Single
    Public Property X1() As Single
        Get
            Return m_X1
        End Get
        Set(ByVal value As Single)
            m_X1 = value
        End Set
    End Property
    Private m_Y1 As Single
    Public Property Y1() As Single
        Get
            Return m_Y1
        End Get
        Set(ByVal value As Single)
            m_Y1 = value
        End Set
    End Property


#End Region

#End Region




#Region "Mouse events"

    Private Sub CtrlCVxx_MouseDown(sender As Object, e As MouseEventArgs) Handles Me.MouseDown
        'mon objet
        m_X1 = MousePosition.X
        m_Y1 = MousePosition.Y


        FuncSnapToGrid(m_X1, m_Y1)

        Off.X = m_X1 - sender.Left
        Off.Y = m_Y1 - sender.Top

    End Sub

    Private Sub CtrlCVxx_MouseMove(sender As Object, e As MouseEventArgs) Handles Me.MouseMove
        'calcule les points de rattachement entre obj 
        '   point bide In
        '   point bride out
        '   point bride top (IP )
        '   point bride bot (ip dans l'autre sens)
        Dim mypointIn, mypointOut, mypointTop, mypointBot As Point
        If e.Button = MouseButtons.Left Then

            m_X2 = MousePosition.X
            m_Y2 = MousePosition.Y
            FuncSnapToGrid(m_X2, m_Y2)
            sender.Left = m_X2 - Off.X
            sender.Top = m_Y2 - Off.Y



            'calcule la position des linkPoint ( pour le moment 4 sont définis in out top bot )

            'in
            mypointIn.X = sender.left
            mypointIn.Y = sender.top + (sender.height / 2)
            v_linkPoint(0)._Point = mypointIn

            'out
            mypointOut.X = sender.left + sender.width
            mypointOut.Y = sender.top + (sender.height / 2)
            v_linkPoint(1)._Point = mypointOut

            'top
            mypointTop.X = sender.left + (sender.width / 2)
            mypointTop.Y = sender.top
            v_linkPoint(2)._Point = mypointTop

            'bot
            mypointBot.X = sender.left + (sender.width / 2)
            mypointBot.Y = sender.top + sender.height
            v_linkPoint(3)._Point = mypointBot

            v_mousemouve = True
            RaiseEvent ObjMooving(v_Index)
        End If

    End Sub

    Private Sub CtrlCVxx_MouseLeave(sender As Object, e As EventArgs) Handles Me.MouseLeave
        Me.BorderStyle = Windows.Forms.BorderStyle.None
        ''PanelCV.BackColor = Color.Gainsboro
        RaiseEvent mymouseleave()

    End Sub



    Private Sub CtrlCVxx_MouseHover(sender As Object, e As EventArgs) Handles Me.MouseHover, PanelCV.MouseHover
        Me.BorderStyle = Windows.Forms.BorderStyle.FixedSingle
        '' PanelCV.BackColor = Color.Red
        RaiseEvent ObjClicked(v_Index)
    End Sub


    Private Sub CtrlCVxx_MouseUp(sender As Object, e As MouseEventArgs) Handles Me.MouseUp
        If e.Button = MouseButtons.Left Then
            RaiseEvent ButtonRealease(v_mousemouve, v_Index)
            v_mousemouve = False
        End If


    End Sub

#End Region


#Region "Proc & Func"
    Private Sub FuncSnapToGrid(ByRef X As Integer, ByRef Y As Integer)
        ' If grid snap is off, do nothing.
        If Not m_SnapToGrid Then Exit Sub

        Dim ix As Integer = CInt(X / m_GridX)
        Dim iy As Integer = CInt(Y / m_GridY)
        X = ix * m_GridX
        Y = iy * m_GridY



    End Sub


#End Region

#Region "menus"


    Private Sub DeleteToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DeleteToolStripMenuItem.Click
        RaiseEvent ObjRemoved(v_Index)
    End Sub

    Private Sub PenningToolStripMenuItem_Click(sender As Object, e As EventArgs)
        'designe de l'obj CH

        TypeObj = eTypObj.PEN
    End Sub



    Private Sub NegCoatingToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles NegCoatingToolStripMenuItem.Click
        NegActive = Not v_negActive

        NegCoatingToolStripMenuItem.Checked = NegActive


    End Sub

    Private Sub LsToolStripMenuItem5_Click(sender As Object, e As EventArgs) Handles LsToolStripMenuItem75l.Click
        ' starcell 75
        pumpingSpeed = 65
        V_volume = 2.3

        RaiseEvent ObjClicked(Index)
    End Sub

    Private Sub LsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles LsToolStripMenuItem150l.Click
        ' starcell 150
        pumpingSpeed = 125
        V_volume = 12.1
        RaiseEvent ObjClicked(Index)
    End Sub



    Private Sub LsToolStripMenuItem2_Click(sender As Object, e As EventArgs) Handles LsToolStripMenuItem300l.Click
        ' starcell 300
        pumpingSpeed = 240
        V_volume = 18.6
        RaiseEvent ObjClicked(Index)
    End Sub



    Private Sub LsToolStripMenuItem4_Click(sender As Object, e As EventArgs) Handles LsToolStripMenuItem500l.Click
        ' starcell 500
        pumpingSpeed = 410
        V_volume = 36.2
        RaiseEvent ObjClicked(Index)

    End Sub
    Private Sub LsToolStripMenuItem6_Click(sender As Object, e As EventArgs) Handles LsToolStripMenuItem20l.Click
        ' starcell 20
        pumpingSpeed = 20
        V_volume = 0.9
        RaiseEvent ObjClicked(Index)
    End Sub
    Private Sub LsToolStripMenuItem7_Click(sender As Object, e As EventArgs) Handles LsToolStripMenuItem55l.Click
        ' starcell 55
        pumpingSpeed = 50
        V_volume = 1.9
        RaiseEvent ObjClicked(Index)
    End Sub

    Private Sub LsToolStripMenuItem8_Click(sender As Object, e As EventArgs) Handles LsToolStripMenuItem40l.Click
        ' starcell 40
        pumpingSpeed = 34
        V_volume = 1.9
        RaiseEvent ObjClicked(Index)
    End Sub
#End Region

    Private Sub CopyToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyToolStripMenuItem.Click
        RaiseEvent ObjCopy(v_Index)
    End Sub




End Class
