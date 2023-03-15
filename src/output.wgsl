@group(0) @binding(0) var<storage> camera: array<u32, 2048>;

struct VertexInput {
    @location(0) position: vec3<f32>,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
};

@vertex
fn vs_main(
    model: VertexInput,
) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = vec4<f32>(model.position, 1.0);
    return out;
}

@fragment
fn fs_main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {
  var color: f32 = 0.;
  color = f32(camera[(i32(pos.x) / 10) + (64 * (i32(pos.y) / 10))] == u32(1));
  return vec4<f32>(color, color, color, 1.0);
}
