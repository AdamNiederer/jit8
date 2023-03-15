use wgpu::util::DeviceExt;
use winit::event_loop::ControlFlow;
use winit::event::{KeyboardInput, Event, WindowEvent, ElementState, VirtualKeyCode};

const VERTICES: &[f32] = &[
    -1., -1., 0.,
    1., -1., 0.,
    -1., 1., 0.,

    -1., 1., 0.,
    1., -1., 0.,
    1., 1., 0.,
];

static DISPLAY_MATRIX: std::sync::RwLock<[u32; 64 * 32]> = std::sync::RwLock::new([1; 64 * 32]);

fn keycode_to_index(key: VirtualKeyCode) -> Option<usize> {
    match key {
        VirtualKeyCode::Key1 => Some(0),
        VirtualKeyCode::Key2 => Some(1),
        VirtualKeyCode::Key3 => Some(2),
        VirtualKeyCode::Q => Some(3),
        VirtualKeyCode::W => Some(4),
        VirtualKeyCode::E => Some(5),
        VirtualKeyCode::A => Some(6),
        VirtualKeyCode::S => Some(7),
        VirtualKeyCode::D => Some(8),
        VirtualKeyCode::Z => Some(9),
        VirtualKeyCode::X => Some(10),
        VirtualKeyCode::C => Some(11),
        _ => None
    }
}

pub async fn run_display() {
    let event_loop = winit::event_loop::EventLoop::new();
    let window = winit::window::WindowBuilder::new()
        .with_inner_size(winit::dpi::PhysicalSize::new(640, 320))
        .build(&event_loop).unwrap();

    let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
        backends: wgpu::Backends::all(),
        dx12_shader_compiler: Default::default(),
    });
    let surface = unsafe { instance.create_surface(&window) }.unwrap();
    let adapter = instance.request_adapter(
        &wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::default(),
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        },
    ).await.unwrap();
    let (device, queue) = adapter.request_device(
        &wgpu::DeviceDescriptor {
            features: wgpu::Features::empty(),
            limits: if cfg!(target_arch = "wasm32") {
                wgpu::Limits::downlevel_webgl2_defaults()
            } else {
                wgpu::Limits::default()
            },
            label: None,
        },
        None,
    ).await.unwrap();
    let surface_caps = surface.get_capabilities(&adapter);
    let surface_format = surface_caps.formats.iter()
        .copied()
        .filter(|f| f.describe().srgb)
        .next()
        .unwrap_or(surface_caps.formats[0]);
    let config = wgpu::SurfaceConfiguration {
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
        format: surface_format,
        width: 640,
        height: 320,
        present_mode: surface_caps.present_modes[0],
        alpha_mode: surface_caps.alpha_modes[0],
        view_formats: vec![],
    };
    surface.configure(&device, &config);

    let shader = device.create_shader_module(wgpu::include_wgsl!("output.wgsl"));

    let vertex_buffer = device.create_buffer_init(
        &wgpu::util::BufferInitDescriptor {
            label: Some("Vertex Buffer"),
            contents: bytemuck::cast_slice(VERTICES),
            usage: wgpu::BufferUsages::VERTEX,
        }
    );

    let display_matrix_buffer = device.create_buffer_init(
        &wgpu::util::BufferInitDescriptor {
            label: Some("Display Matrix Buffer"),
            contents: bytemuck::cast_slice(DISPLAY_MATRIX.read().unwrap().as_slice()),
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
        }
    );

    let display_matrix_bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        entries: &[
            wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Storage { read_only: true },
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            }
        ],
        label: Some("display_matrix_bind_group_layout"),
    });

    let display_matrix_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        layout: &display_matrix_bind_group_layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: display_matrix_buffer.as_entire_binding(),
            }
        ],
        label: Some("display_matrix_bind_group"),
    });


    let vertex_buffer_layout = wgpu::VertexBufferLayout {
        array_stride: std::mem::size_of::<[f32; 3]>() as wgpu::BufferAddress,
        step_mode: wgpu::VertexStepMode::Vertex,
        attributes: &wgpu::vertex_attr_array![0 => Float32x3],
    };

    let render_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: Some("Render Pipeline Layout"),
        bind_group_layouts: &[&display_matrix_bind_group_layout],
        push_constant_ranges: &[],
    });

    let render_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some("Render Pipeline"),
        layout: Some(&render_pipeline_layout),
        vertex: wgpu::VertexState {
            module: &shader,
            entry_point: "vs_main",
            buffers: &[vertex_buffer_layout],
        },
        fragment: Some(wgpu::FragmentState {
            module: &shader,
            entry_point: "fs_main",
            targets: &[Some(wgpu::ColorTargetState {
                format: config.format,
                blend: Some(wgpu::BlendState::REPLACE),
                write_mask: wgpu::ColorWrites::ALL,
            })],
        }),
        primitive: wgpu::PrimitiveState {
            topology: wgpu::PrimitiveTopology::TriangleList,
            strip_index_format: None,
            front_face: wgpu::FrontFace::Ccw,
            cull_mode: Some(wgpu::Face::Back),
            polygon_mode: wgpu::PolygonMode::Fill,
            unclipped_depth: false,
            conservative: false,
        },
        depth_stencil: None,
        multisample: wgpu::MultisampleState {
            count: 1,
            mask: !0,
            alpha_to_coverage_enabled: false,
        },
        multiview: None,
    });

    event_loop.run(move |event, _, control_flow| {
        match event {
            Event::WindowEvent { ref event, window_id, } if window_id == window.id() => {
                match event {
                    WindowEvent::CloseRequested | WindowEvent::KeyboardInput {
                        input: KeyboardInput {
                            state: ElementState::Pressed,
                            virtual_keycode: Some(VirtualKeyCode::Escape),
                            ..
                        },
                        ..
                    } => *control_flow = ControlFlow::Exit,
                    WindowEvent::KeyboardInput {
                        input: KeyboardInput { state: ElementState::Pressed, .. },
                        ..
                    } => {
                        let first = DISPLAY_MATRIX.read().unwrap()[0];
                        if let Ok(mut writer) = DISPLAY_MATRIX.try_write() {
                            for i in 0..1024 {
                                writer[i * 2] = if first == 0 { 1 } else { 0 };
                            }
                        }
                        queue.write_buffer(&display_matrix_buffer, 0, bytemuck::cast_slice(DISPLAY_MATRIX.read().unwrap().as_slice()));
                        window.request_redraw();
                    }
                    _ => {}
                }
            }
            Event::RedrawRequested(window_id) if window_id == window.id() => {
                let output = surface.get_current_texture().unwrap();
                let view = output.texture.create_view(&wgpu::TextureViewDescriptor::default());
                let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some("Render Encoder"),
                });
                let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                    label: Some("Render Pass"),
                    color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                        view: &view,
                        resolve_target: None,
                        ops: wgpu::Operations {
                            load: wgpu::LoadOp::Clear(wgpu::Color {
                                r: 0.1,
                                g: 0.2,
                                b: 0.3,
                                a: 1.0,
                            }),
                            store: true,
                        },
                    })],
                    depth_stencil_attachment: None,
                });
                render_pass.set_pipeline(&render_pipeline);
                render_pass.set_bind_group(0, &display_matrix_bind_group, &[]);
                render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
                render_pass.draw(0..6, 0..1);
                std::mem::drop(render_pass);
                queue.submit(std::iter::once(encoder.finish()));
                output.present();
            },
            Event::MainEventsCleared => { window.request_redraw(); },
            _ => {}
        }
    });
}
