export const copyToClipboard_ = ({ ok, error }) => (text) => async () => {
  try {
    await navigator.clipboard.writeText(text)
    return ok
  } catch (e) {
    return error(e)
  }
}

export const readFromClipboard_ = ({ ok, error }) => async () => {
  try {
    const text = await navigator.clipboard.readText();
    return ok(text);
  } catch (e) {
    return error(e);
  }
}

