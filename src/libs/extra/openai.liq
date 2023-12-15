let error.openai = error.register("openai")

def chatgpt(~key, ~model="gpt-3.5-turbo", messages) =
  payload = {model=model, messages=messages}

  ans =
    http.post(
      data=json.stringify(payload),
      headers=
        [
          ("Content-Type", "application/json"),
          (
            "Authorization",
            "Bearer #{(key : string)}"
          )
        ],
      "https://api.openai.com/v1/chat/completions"
    )
  try
    let json.parse (ans :
      {
        choices: [
          {
            finish_reason: string,
            index: int,
            message: {content: string, role: string}
          }
        ],
        created: int,
        model: string,
        object: string,
        usage: {completion_tokens: int, prompt_tokens: int, total_tokens: int}
      }
    ) = ans
    ans
  catch _ : [error.json] do
    let json.parse (e :
      {error: {message: string, type: string, code: string}}
    ) = ans
    e = e.error
    error.raise(
      error.openai,
      "#{e.code}: #{e.message}"
    )
  end
end

# ChatGPT response to a prompt.
def chatgpt.response(
  ~key,
  ~model="gpt-3.5-turbo",
  ~system="You are a helpful assistant.",
  prompt
) =
  messages = [{role="system", content=system}, {role="user", content=prompt}]
  ans = chatgpt(key=key, model=model, messages)
  ans
end

openai = ()

# Speech-to-text.
# @param ~key OpenAI API key.
# @param filename Mp3 file containing speech.
def openai.whisper(~key, filename) =
  # http.post(
  # headers=[
  # ("Authorization", "Bearer #{key}"),
  # ("Content-Type", "multipart/form-data")
  # ],
  # "https://api.openai.com/v1/audio/transcriptions")
  error.raise(error.openai("TODO"))
end
